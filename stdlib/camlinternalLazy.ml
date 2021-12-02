(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Internals of forcing lazy values. *)

type 'a t = 'a lazy_t
(*
  =
  | Immediate (* short-circuited by GC *)
  | Block_with_regular_tag of Obj.t (* short-circuited by GC *)
  (* ... *)
  | Lazy_tag of { mutable thunk : 'a thunk_status ;
                  mutable result : 'a }
*)

type 'a thunk_status =
  | Forcing
  | Forward (* Must be different from () to avoid any race between
               initialization and shortcutting by the GC *)
  | Thunk of Obj.t (* Inlined by hand for the time being, same trick
                      as in camlinternalOO.ml *)

let atomic_thunk_field : 'a t -> 'a thunk_status Atomic.t = Obj.magic

exception Undefined

(* Expects that lzv is locked with Forward bit, and we own the lock. *)
let force_gen_lazy_block ~only_val (lzv : 'arg lazy_t) (thunk : unit -> 'arg) =
  let thunk_status = atomic_thunk_field lzv in
  match thunk () with
  | result -> begin
      Obj.set_field (Obj.repr lzv) 1 (Obj.repr result) ;
      (* We only need a release store *)
      Atomic.set thunk_status Forward ;
      result
    end
  | exception e when not only_val -> begin
      let repeat () = raise e in
      (* No synchronisation needed, but performance does not matter
         here. *)
      Atomic.set thunk_status (Obj.magic repeat) ;
      raise e
    end

(* used in the %lazy_force primitive *)
let force_lazy_block blk f = force_gen_lazy_block ~only_val:false blk f

(* [force_gen ~only_val:false] is not used, since [Lazy.force] is
   declared as a primitive whose code inlines the tag tests of its
   argument, except when afl instrumentation is turned on. *)
let force_gen ~only_val (lzv : 'arg lazy_t) =
  (* Using [Sys.opaque_identity] prevents two potential problems:
     - If the value is known to have Lazy_tag, then its tag could have
       changed during GC, so that information must be forgotten (see GPR#713
       and issue #7301)
     - If the value is known to be immutable, then if the compiler
       cannot prove that the forcing branch is not taken it will issue a
       warning 59 (modification of an immutable value) *)
  let lzv = Sys.opaque_identity lzv in
  let x = Obj.repr lzv in
  (* START no polling points. If a GC occurs here, then the object [x]
     may be short-circuited, and getting the first field of [x] would
     get us the wrong value. Luckily, the compiler does not insert GC
     safe points at this place, so it is ok. *)
  let t = Obj.tag x in
  if t = Obj.lazy_tag then
    let thunk_status = atomic_thunk_field lzv in
    (* Synchronize the read of Forward with the read of the field.
       Here we only need an acquire fence inside the Forward
       branch. *)
    match Atomic.get thunk_status with
    | Forward ->
        (Obj.obj (Obj.field x 1) : 'arg)
        (* END no polling points *)
    | Forcing -> raise Undefined
    | f -> begin
        if not (Atomic.compare_and_set thunk_status f Forcing) then
          raise Lazy.Undefined
        else
          (* END no polling points: we now hold a lock on the lazy
             block, so it keeps tag Lazy_tag until we change the thunk
             status to Forward. *)
          let f : unit -> 'arg = Obj.magic f in
          force_gen_lazy_block ~only_val lzv f
      end
  else
    (Obj.obj x : 'arg)

let make_forward (x : 'a) =
  let block = Obj.new_block Obj.lazy_tag 2 in
  Obj.set_field block 1 (Obj.repr x) ;
  let block = Sys.opaque_identity block in
  Obj.set_field block 0 (Obj.repr Forward) ;
  (Obj.obj block : 'a lazy_t)

let is_val (lzv : 'arg t) =
  let lzv = Sys.opaque_identity lzv in
  let x = Obj.repr lzv in
  (* START no polling points. *)
  let t = Obj.tag x in
  (t <> Obj.lazy_tag)
  || (let thunk_status = (Obj.obj (Obj.field x 0) : 'a thunk_status) in
      (* END no polling points *)
      thunk_status = Forward)
