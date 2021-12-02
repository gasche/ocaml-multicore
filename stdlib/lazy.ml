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

(* Module [Lazy]: deferred computations *)


(*
   WARNING: some purple magic is going on here.  Do not take this file
   as an example of how to program in OCaml.
*)


(* We make use of a special tag provided by the runtime, [lazy_tag].

   A value of type ['a Lazy.t] can be one of four things:
   1. A block of size 2 with tag [lazy_tag], and:
   1a. the first field is a closure of type [unit -> 'a] that computes
       the value, and the second field is ready to receive a value.
   1b. the first field is Forward, and the second field is the value
       of type ['a] that was computed.
   1c. the first field is Forcing, which denotes a block which is
       locked by a thread until it receives a value.
   2. Anything else except a float. This has type ['a] and is the
      value that was computed.
   Exceptions are stored in format (1a).
   The GC will magically change things from (1b) to (2) according to its
   fancy.

   If OCaml was configured with the -flat-float-array option (which is
   currently the default), the following is also true:
   We cannot use representation (2) for a [float Lazy.t] because
   [caml_make_array] assumes that only a [float] value can have tag
   [Double_tag].

   We have to use the built-in type constructor [lazy_t] to
   let the compiler implement the special typing and compilation
   rules for the [lazy] keyword.
*)

type 'a t = 'a CamlinternalLazy.t

exception Undefined = CamlinternalLazy.Undefined

external force : 'a t -> 'a = "%lazy_force"

let force_val l = CamlinternalLazy.force_gen ~only_val:true l

let from_fun (f : unit -> 'arg) =
  let x = Obj.new_block Obj.lazy_tag 1 in
  Obj.set_field x 0 (Obj.repr f) ;
  Obj.set_field x 1 (Obj.repr ()) ;
  (Obj.obj x : 'arg t)

let make_forward = CamlinternalLazy.make_forward

let from_val (v : 'arg) =
  let t = Obj.tag (Obj.repr v) in
  if t = Obj.lazy_tag || t = Obj.double_tag then
    make_forward v
  else
    (Obj.magic v : 'arg t)

let is_val = CamlinternalLazy.is_val

(* Below are deprecated *)

let lazy_from_fun = from_fun

let lazy_from_val = from_val

let lazy_is_val = is_val

let map f x =
  lazy (f (force x))

let map_val f x =
  if is_val x
  then lazy_from_val (f (force x))
  else lazy (f (force x))
