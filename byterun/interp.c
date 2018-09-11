/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* The bytecode interpreter */
#include <stdio.h>
#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/callback.h"
#include "caml/debugger.h"
#include "caml/fail.h"
#include "caml/fix_code.h"
#include "caml/instrtrace.h"
#include "caml/instruct.h"
#include "caml/interp.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/prims.h"
#include "caml/signals.h"
#include "caml/fiber.h"
#include "caml/domain.h"
#include "caml/globroots.h"
#include "caml/startup.h"
#include "caml/startup_aux.h"

/* Registers for the abstract machine:
        pc         the code pointer
        sp         the stack pointer (grows downward)
        accu       the accumulator
        env        heap-allocated environment
        caml_trapsp pointer to the current trap frame
        extra_args number of extra arguments provided by the caller

sp is a local copy of the global variable caml_extern_sp. */

/* Instruction decoding */

#ifdef THREADED_CODE
#  define Instruct(name) lbl_##name
#  if defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
#    define Jumptbl_base ((char *) &&lbl_ACC0)
#  else
#    define Jumptbl_base ((char *) 0)
#    define jumptbl_base ((char *) 0)
#  endif
#  ifdef DEBUG
#    define Next goto next_instr
#  else
#    define Next goto *(void *)(jumptbl_base + *pc++)
#  endif
#else
#  define Instruct(name) case name
#  define Next break
#endif

/* GC interface */

#define Setup_for_gc \
  { sp -= 2; sp[0] = accu; sp[1] = env; domain_state->current_stack->sp = sp; }
#define Restore_after_gc \
  { sp = domain_state->current_stack->sp; accu = sp[0]; env = sp[1]; sp += 2; }
#define Enter_gc \
  { Setup_for_gc; caml_handle_gc_interrupt(); Restore_after_gc; }
#define Setup_for_c_call \
  { saved_pc = pc; *--sp = env; domain_state->current_stack->sp = sp; }
#define Restore_after_c_call \
  { sp = domain_state->current_stack->sp; env = *sp++; saved_pc = NULL; }

/* An event frame must look like accu + a C_CALL frame + a RETURN 1 frame */
#define Setup_for_event \
  { sp -= 6; \
    sp[0] = accu; /* accu */ \
    sp[1] = Val_unit; /* C_CALL frame: dummy environment */ \
    sp[2] = Val_unit; /* RETURN frame: dummy local 0 */ \
    sp[3] = Val_pc(pc); /* RETURN frame: saved return address */  \
    sp[4] = env; /* RETURN frame: saved environment */ \
    sp[5] = Val_long(extra_args); /* RETURN frame: saved extra args */ \
    domain_state->current_stack->sp = sp; }
#define Restore_after_event \
  { sp = domain_state->current_stack->sp; accu = sp[0]; \
    pc = Pc_val(sp[3]); env = sp[4]; extra_args = Long_val(sp[5]); \
    sp += 6; }

/* Debugger interface */

#define Setup_for_debugger \
   { sp -= 4; \
     sp[0] = accu; sp[1] = (value)(pc - 1); \
     sp[2] = env; sp[3] = Val_long(extra_args); \
     domain_state->current_stack->sp = sp; }
#define Restore_after_debugger { sp += 4; }

#ifdef THREADED_CODE
#define Restart_curr_instr \
  goto *(jumptable[caml_saved_code[pc - 1 - caml_start_code]])
#else
#define Restart_curr_instr \
  curr_instr = caml_saved_code[pc - 1 - caml_start_code]; \
  goto dispatch_instr
#endif


/* Initialising fields of objects just allocated with Alloc_small */
#define Init_field(o, i, x) Op_val(o)[i] = (x)

/* Inlined read barrier for interpreter loop.
   Does 'accu = accu[i]' */
#define Accu_field(i) do {                    \
    int idx = (i);                            \
    value field_contents = Op_val(accu)[idx]; \
    if (Is_foreign(field_contents)) {         \
      Setup_for_gc;                           \
      value r = caml_read_barrier(accu, idx); \
      Restore_after_gc;                       \
      accu = r;                               \
    } else {                                  \
      accu = field_contents;                  \
    }                                         \
  } while (0)

/* Register optimization.
   Some compilers underestimate the use of the local variables representing
   the abstract machine registers, and don't put them in hardware registers,
   which slows down the interpreter considerably.
   For GCC, I have hand-assigned hardware registers for several architectures.
*/

#if defined(__GNUC__) && !defined(DEBUG) && !defined(__INTEL_COMPILER) \
    && !defined(__llvm__)
#ifdef __mips__
#define PC_REG asm("$16")
#define SP_REG asm("$17")
#define ACCU_REG asm("$18")
#endif
#ifdef __sparc__
#define PC_REG asm("%l0")
#define SP_REG asm("%l1")
#define ACCU_REG asm("%l2")
#endif
#ifdef __alpha__
#ifdef __CRAY__
#define PC_REG asm("r9")
#define SP_REG asm("r10")
#define ACCU_REG asm("r11")
#define JUMPTBL_BASE_REG asm("r12")
#else
#define PC_REG asm("$9")
#define SP_REG asm("$10")
#define ACCU_REG asm("$11")
#define JUMPTBL_BASE_REG asm("$12")
#endif
#endif
#ifdef __i386__
#define PC_REG asm("%esi")
#define SP_REG asm("%edi")
#define ACCU_REG
#endif
#if defined(__ppc__) || defined(__ppc64__)
#define PC_REG asm("26")
#define SP_REG asm("27")
#define ACCU_REG asm("28")
#endif
#ifdef __hppa__
#define PC_REG asm("%r18")
#define SP_REG asm("%r17")
#define ACCU_REG asm("%r16")
#endif
#ifdef __mc68000__
#define PC_REG asm("a5")
#define SP_REG asm("a4")
#define ACCU_REG asm("d7")
#endif
/* PR#4953: these specific registers not available in Thumb mode */
#if defined (__arm__) && !defined(__thumb__)
#define PC_REG asm("r6")
#define SP_REG asm("r8")
#define ACCU_REG asm("r7")
#endif
#ifdef __ia64__
#define PC_REG asm("36")
#define SP_REG asm("37")
#define ACCU_REG asm("38")
#define JUMPTBL_BASE_REG asm("39")
#endif
#ifdef __x86_64__
#define PC_REG asm("%r15")
#define SP_REG asm("%r14")
#define ACCU_REG asm("%r13")
#endif
#ifdef __aarch64__
#define PC_REG asm("%x19")
#define SP_REG asm("%x20")
#define ACCU_REG asm("%x21")
#define JUMPTBL_BASE_REG asm("%x22")
#endif
#endif

#ifdef DEBUG
static __thread intnat caml_bcodcount;
#endif

static caml_root raise_unhandled;

/* The interpreter itself */
value caml_interprete(code_t prog, asize_t prog_size)
{
#ifdef PC_REG
  register code_t pc PC_REG;
  register value * sp SP_REG;
  register value accu ACCU_REG;
#else
  register code_t pc;
  register value * sp;
  register value accu;
#endif
#if defined(THREADED_CODE) && defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
#ifdef JUMPTBL_BASE_REG
  register char * jumptbl_base JUMPTBL_BASE_REG;
#else
  register char * jumptbl_base;
#endif
#endif
  value env;
  intnat extra_args;
  struct caml_exception_context * initial_external_raise;
  int initial_stack_words;
  intnat initial_trap_sp_off;
  volatile code_t saved_pc = NULL;
  volatile value raise_exn_bucket = Val_unit;
  struct longjmp_buffer raise_buf;
  caml_domain_state* domain_state = Caml_state;
  struct caml_exception_context exception_ctx =
    { &raise_buf, domain_state->local_roots, &raise_exn_bucket};
#ifndef THREADED_CODE
  opcode_t curr_instr;
#endif

#ifdef THREADED_CODE
  static void * jumptable[] = {
#    include "caml/jumptbl.h"
  };
#endif

  if (prog == NULL) {           /* Interpreter is initializing */
    static opcode_t raise_unhandled_code[] = { ACC, 0, RAISE };
#ifdef THREADED_CODE
    caml_instr_table = (char **) jumptable;
    caml_instr_base = Jumptbl_base;
    caml_thread_code(raise_unhandled_code,
                     sizeof(raise_unhandled_code));
#endif
    value raise_unhandled_closure =
      caml_alloc_1(Closure_tag,
                   Val_bytecode(raise_unhandled_code));
    raise_unhandled = caml_create_root(raise_unhandled_closure);
    caml_global_data = caml_create_root(Val_unit);
    caml_init_callbacks();
    return Val_unit;
  }

#if defined(THREADED_CODE) && defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
  jumptbl_base = Jumptbl_base;
#endif
  initial_trap_sp_off = domain_state->trap_sp_off;
  initial_stack_words = Stack_high(domain_state->current_stack) - domain_state->current_stack->sp;
  initial_external_raise = domain_state->external_raise;
  caml_incr_callback_depth ();
  saved_pc = NULL;

  if (sigsetjmp(raise_buf.buf, 0)) {
    /* no non-volatile local variables read here */
    sp = domain_state->current_stack->sp;
    accu = raise_exn_bucket;
    pc = saved_pc; saved_pc = NULL;
    if (pc != NULL) pc += 2;
        /* +2 adjustement for the sole purpose of backtraces */
    goto raise_exception;
  }
  domain_state->external_raise = &exception_ctx;

  domain_state->trap_sp_off = 1;

  sp = domain_state->current_stack->sp;
  pc = prog;
  extra_args = 0;
  env = Atom(0);
  accu = Val_int(0);


#ifdef THREADED_CODE
#ifdef DEBUG
 next_instr:
  if (caml_icount-- == 0) caml_stop_here ();
  Assert(Stack_base(domain_state->current_stack) <= sp &&
         sp <= Stack_high(domain_state->current_stack));
#endif
  goto *(void *)(jumptbl_base + *pc++); /* Jump to the first instruction */
#else
  while(1) {
#ifdef DEBUG

    Assert(!Is_foreign(accu));
    Assert(!Is_foreign(env));

    caml_bcodcount++;
    if (caml_icount-- == 0) caml_stop_here ();
    if (caml_params->trace_level>1) printf("\n##%ld\n", caml_bcodcount);
    if (caml_params->trace_level) caml_disasm_instr(pc);
    if (caml_params->trace_level>1) {
      printf("env=");
      caml_trace_value_file(env,prog,prog_size,stdout);
      putchar('\n');
      caml_trace_accu_sp_file(accu,sp,prog,prog_size,stdout);
      fflush(stdout);
    };
    Assert(Stack_base(domain_state->current_stack) <= sp &&
           sp <= Stack_high(domain_state->current_stack));

#endif
    curr_instr = *pc++;

  dispatch_instr:
    switch(curr_instr) {
#endif

/* Basic stack operations */

    Instruct(ACC0):
      accu = sp[0]; Next;
    Instruct(ACC1):
      accu = sp[1]; Next;
    Instruct(ACC2):
      accu = sp[2]; Next;
    Instruct(ACC3):
      accu = sp[3]; Next;
    Instruct(ACC4):
      accu = sp[4]; Next;
    Instruct(ACC5):
      accu = sp[5]; Next;
    Instruct(ACC6):
      accu = sp[6]; Next;
    Instruct(ACC7):
      accu = sp[7]; Next;

    Instruct(PUSH): Instruct(PUSHACC0):
      *--sp = accu; Next;
    Instruct(PUSHACC1):
      *--sp = accu; accu = sp[1]; Next;
    Instruct(PUSHACC2):
      *--sp = accu; accu = sp[2]; Next;
    Instruct(PUSHACC3):
      *--sp = accu; accu = sp[3]; Next;
    Instruct(PUSHACC4):
      *--sp = accu; accu = sp[4]; Next;
    Instruct(PUSHACC5):
      *--sp = accu; accu = sp[5]; Next;
    Instruct(PUSHACC6):
      *--sp = accu; accu = sp[6]; Next;
    Instruct(PUSHACC7):
      *--sp = accu; accu = sp[7]; Next;

    Instruct(PUSHACC):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ACC):
      accu = sp[*pc++];
      Next;

    Instruct(POP):
      sp += *pc++;
      Next;
    Instruct(ASSIGN):
      sp[*pc++] = accu;
      accu = Val_unit;
      Next;

/* Access in heap-allocated environment */

    Instruct(ENVACC1):
      accu = Field_imm(env, 1); Next;
    Instruct(ENVACC2):
      accu = Field_imm(env, 2); Next;
    Instruct(ENVACC3):
      accu = Field_imm(env, 3); Next;
    Instruct(ENVACC4):
      accu = Field_imm(env, 4); Next;

    Instruct(PUSHENVACC1):
      *--sp = accu; accu = Field_imm(env, 1); Next;
    Instruct(PUSHENVACC2):
      *--sp = accu; accu = Field_imm(env, 2); Next;
    Instruct(PUSHENVACC3):
      *--sp = accu; accu = Field_imm(env, 3); Next;
    Instruct(PUSHENVACC4):
      *--sp = accu; accu = Field_imm(env, 4); Next;

    Instruct(PUSHENVACC):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ENVACC):
      accu = Field_imm(env, *pc++);
      Next;

/* Function application */

    Instruct(PUSH_RETADDR): {
      sp -= 3;
      sp[0] = Val_pc(pc + *pc);
      sp[1] = env;
      sp[2] = Val_long(extra_args);
      pc++;
      Next;
    }
    Instruct(APPLY): {
      extra_args = *pc - 1;
      pc = Code_val(accu);
      env = accu;
      goto check_stacks;
    }
    Instruct(APPLY1): {
      value arg1 = sp[0];
      sp -= 3;
      sp[0] = arg1;
      sp[1] = Val_pc(pc);
      sp[2] = env;
      sp[3] = Val_long(extra_args);
      pc = Code_val(accu);
      env = accu;
      extra_args = 0;
      goto check_stacks;
    }
    Instruct(APPLY2): {
      value arg1 = sp[0];
      value arg2 = sp[1];
      sp -= 3;
      sp[0] = arg1;
      sp[1] = arg2;
      sp[2] = Val_pc(pc);
      sp[3] = env;
      sp[4] = Val_long(extra_args);
      pc = Code_val(accu);
      env = accu;
      extra_args = 1;
      goto check_stacks;
    }
    Instruct(APPLY3): {
      value arg1 = sp[0];
      value arg2 = sp[1];
      value arg3 = sp[2];
      sp -= 3;
      sp[0] = arg1;
      sp[1] = arg2;
      sp[2] = arg3;
      sp[3] = Val_pc(pc);
      sp[4] = env;
      sp[5] = Val_long(extra_args);
      pc = Code_val(accu);
      env = accu;
      extra_args = 2;
      goto check_stacks;
    }

    Instruct(APPTERM): {
      int nargs = *pc++;
      int slotsize = *pc;
      value * newsp;
      int i;
      /* Slide the nargs bottom words of the current frame to the top
         of the frame, and discard the remainder of the frame */
      newsp = sp + slotsize - nargs;
      for (i = nargs - 1; i >= 0; i--) newsp[i] = sp[i];
      sp = newsp;
      pc = Code_val(accu);
      env = accu;
      extra_args += nargs - 1;
      goto check_stacks;
    }
    Instruct(APPTERM1): {
      value arg1 = sp[0];
      sp = sp + *pc - 1;
      sp[0] = arg1;
      pc = Code_val(accu);
      env = accu;
      goto check_stacks;
    }
    Instruct(APPTERM2): {
      value arg1 = sp[0];
      value arg2 = sp[1];
      sp = sp + *pc - 2;
      sp[0] = arg1;
      sp[1] = arg2;
      pc = Code_val(accu);
      env = accu;
      extra_args += 1;
      goto check_stacks;
    }
    Instruct(APPTERM3): {
      value arg1 = sp[0];
      value arg2 = sp[1];
      value arg3 = sp[2];
      sp = sp + *pc - 3;
      sp[0] = arg1;
      sp[1] = arg2;
      sp[2] = arg3;
      pc = Code_val(accu);
      env = accu;
      extra_args += 2;
      goto check_stacks;
    }

    Instruct(RETURN): {
      sp += *pc++;
      if (extra_args > 0) {
        extra_args--;
        pc = Code_val(accu);
        env = accu;
        Next;
      } else {
        goto do_return;
      }
    }

    do_return:
      if (sp == Stack_high(domain_state->current_stack)) {
        /* return to parent stack */
        struct stack_info* parent_stack =
          Stack_parent(domain_state->current_stack);
        value hval = Stack_handle_value(domain_state->current_stack);
        Assert(parent_stack != NULL);

        domain_state->current_stack->sp = sp;
        struct stack_info* old_stack = caml_switch_stack(parent_stack);
        sp = domain_state->current_stack->sp;
        caml_free_stack(old_stack);

        domain_state->trap_sp_off = Long_val(sp[0]);
        extra_args = Long_val(sp[1]);
        sp++;
        sp[0] = accu;

        accu = hval;
        pc = Code_val(accu);
        env = accu;
        goto check_stacks;
      } else {
        /* return to callee, no stack switching */
        pc = Pc_val(sp[0]);
        env = sp[1];
        extra_args = Long_val(sp[2]);
        sp += 3;
      }
      Next;

    Instruct(RESTART): {
      int num_args = Wosize_val(env) - 2;
      int i;
      sp -= num_args;
      for (i = 0; i < num_args; i++) sp[i] = Field_imm(env, i + 2);
      env = Field_imm(env, 1);
      extra_args += num_args;
      Next;
    }

    Instruct(GRAB): {
      int required = *pc++;
      if (extra_args >= required) {
        extra_args -= required;
        Next;
      } else {
        mlsize_t num_args, i;
        num_args = 1 + extra_args; /* arg1 + extra args */
        Alloc_small(accu, num_args + 2, Closure_tag, Enter_gc);
        Init_field(accu, 1, env);
        for (i = 0; i < num_args; i++) Init_field(accu, i + 2, sp[i]);
        Init_field(accu, 0, Val_bytecode(pc - 3)); /* Point to the preceding RESTART instr. */
        sp += num_args;
        goto do_return;
      }
    }

    Instruct(CLOSURE): {
      int nvars = *pc++;
      int i;
      if (nvars > 0) *--sp = accu;
      if (nvars < Max_young_wosize) {
        /* nvars + 1 <= Max_young_wosize, can allocate in minor heap */
        Alloc_small(accu, 1 + nvars, Closure_tag, Enter_gc);
        Init_field(accu, 0, Val_bytecode(pc + *pc));
        for (i = 0; i < nvars; i++) Init_field(accu, i + 1, sp[i]);
      } else {
        /* PR#6385: must allocate in major heap */
        accu = caml_alloc_shr(1 + nvars, Closure_tag);
        Setup_for_c_call;
        caml_initialize_field(accu, 0, Val_bytecode(pc + *pc));
        Restore_after_c_call;
        for (i = 0; i < nvars; i++) {
          value v = sp[i];
          Setup_for_c_call;
          caml_initialize_field(accu, i + 1, v);
          Restore_after_c_call;
        }
      }
      pc++;
      sp += nvars;
      Next;
    }

    Instruct(CLOSUREREC): {
      int nfuncs = *pc++;
      int nvars = *pc++;
      int i, field;
      int var_offset = nfuncs * 2 - 1;
      int blksize = var_offset + nvars;
      if (nvars > 0) *--sp = accu;
      if (blksize <= Max_young_wosize) {
        Alloc_small(accu, blksize, Closure_tag, Enter_gc);
        for (i = 0; i < nvars; i++) {
          Init_field(accu, var_offset + i, sp[i]);
        }
        Caml_state->youngest_escaped = accu;
      } else {
        /* PR#6385: must allocate in major heap */
        accu = caml_alloc_shr(blksize, Closure_tag);
        for (i = 0; i < nvars; i++) {
          value v = sp[i];
          Setup_for_c_call;
          caml_initialize_field(accu, var_offset + i, v);
          Restore_after_c_call;
        }
      }
      sp += nvars;
      /* The code pointers and infix headers are not in the heap,
         so no need to go through caml_initialize. */
      Init_field(accu, 0, Val_bytecode(pc + pc[0]));
      *--sp = accu;
      field = 1;
      for (i = 1; i < nfuncs; i++) {
        Init_field(accu, field, Make_header(i * 2, Infix_tag, 0)); /* color irrelevant */
        field++;
        Init_field(accu, field, Val_bytecode (pc + pc[i]));
        *--sp = (value) (Op_val(accu) + field);
        field++;
      }
      pc += nfuncs;
      Next;
    }

    Instruct(PUSHOFFSETCLOSURE):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSURE):
      accu = env + *pc++ * sizeof(value); Next;

    Instruct(PUSHOFFSETCLOSUREM2):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSUREM2):
      accu = env - 2 * sizeof(value); Next;
    Instruct(PUSHOFFSETCLOSURE0):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSURE0):
      accu = env; Next;
    Instruct(PUSHOFFSETCLOSURE2):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSURE2):
      accu = env + 2 * sizeof(value); Next;


/* Access to global variables */

    Instruct(PUSHGETGLOBAL):
      *--sp = accu;
      /* Fallthrough */
    Instruct(GETGLOBAL):
      accu = Field_imm(caml_read_root(caml_global_data), *pc);
      pc++;
      Next;

    Instruct(PUSHGETGLOBALFIELD):
      *--sp = accu;
      /* Fallthrough */
    Instruct(GETGLOBALFIELD): {
      accu = Field_imm(caml_read_root(caml_global_data), *pc);
      pc++;
      Accu_field(*pc);
      pc++;
      Next;
    }

    Instruct(SETGLOBAL):
      accu = caml_promote(caml_domain_self(), accu);
      caml_modify_field(caml_read_root(caml_global_data), *pc, accu);
      accu = Val_unit;
      pc++;
      Next;

/* Allocation of blocks */

    Instruct(PUSHATOM0):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ATOM0):
      accu = Atom(0); Next;

    Instruct(PUSHATOM):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ATOM):
      accu = Atom(*pc++); Next;

    Instruct(MAKEBLOCK): {
      mlsize_t wosize = *pc++;
      tag_t tag = *pc++;
      mlsize_t i;
      value block;
      if (wosize <= Max_young_wosize) {
        Alloc_small(block, wosize, tag, Enter_gc);
        Init_field(block, 0, accu);
        for (i = 1; i < wosize; i++) Init_field(block, i, *sp++);
      } else {
        block = caml_alloc_shr(wosize, tag);
        Setup_for_c_call;
        caml_initialize_field(block, 0, accu);
        Restore_after_c_call;
        for (i = 1; i < wosize; i++) {
          value v = *sp++;
          Setup_for_c_call;
          caml_initialize_field(block, i, v);
          Restore_after_c_call;
        }
      }
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK1): {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 1, tag, Enter_gc);
      Init_field(block, 0, accu);
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK2): {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 2, tag, Enter_gc);
      Init_field(block, 0, accu);
      Init_field(block, 1, sp[0]);
      sp += 1;
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK3): {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 3, tag, Enter_gc);
      Init_field(block, 0, accu);
      Init_field(block, 1, sp[0]);
      Init_field(block, 2, sp[1]);
      sp += 2;
      accu = block;
      Next;
    }
    Instruct(MAKEFLOATBLOCK): {
      mlsize_t size = *pc++;
      mlsize_t i;
      value block;
      if (size <= Max_young_wosize / Double_wosize) {
        Alloc_small(block, size * Double_wosize, Double_array_tag, Enter_gc);
      } else {
        Setup_for_gc;
        block = caml_alloc_shr(size * Double_wosize, Double_array_tag);
        Restore_after_gc;
      }
      Store_double_flat_field(block, 0, Double_val(accu));
      for (i = 1; i < size; i++){
        Store_double_flat_field(block, i, Double_val(*sp));
        ++ sp;
      }
      accu = block;
      Next;
    }

/* Access to components of blocks */

    Instruct(GETFIELD0):
      accu = Field_imm(accu, 0); Next;
    Instruct(GETFIELD1):
      accu = Field_imm(accu, 1); Next;
    Instruct(GETFIELD2):
      accu = Field_imm(accu, 2); Next;
    Instruct(GETFIELD3):
      accu = Field_imm(accu, 3); Next;
    Instruct(GETFIELD):
      accu = Field_imm(accu, *pc); pc++; Next;
    Instruct(GETMUTABLEFIELD0):
      Accu_field(0); Next;
    Instruct(GETMUTABLEFIELD1):
      Accu_field(1); Next;
    Instruct(GETMUTABLEFIELD2):
      Accu_field(2); Next;
    Instruct(GETMUTABLEFIELD3):
      Accu_field(3); Next;
    Instruct(GETMUTABLEFIELD):
      Accu_field(*pc); pc++; Next;
    Instruct(GETFLOATFIELD): {
      double d = Double_flat_field(accu, *pc);
      Alloc_small(accu, Double_wosize, Double_tag, Enter_gc);
      Store_double_val(accu, d);
      pc++;
      Next;
    }

    Instruct(SETFIELD0):
      caml_modify_field(accu, 0, *sp++);
      accu = Val_unit;
      Next;
    Instruct(SETFIELD1):
      caml_modify_field(accu, 1, *sp++);
      accu = Val_unit;
      Next;
    Instruct(SETFIELD2):
      caml_modify_field(accu, 2, *sp++);
      accu = Val_unit;
      Next;
    Instruct(SETFIELD3):
      caml_modify_field(accu, 3, *sp++);
      accu = Val_unit;
      Next;
    Instruct(SETFIELD):
      caml_modify_field(accu, *pc, *sp++);
      accu = Val_unit;
      pc++;
      Next;
    Instruct(SETFLOATFIELD):
      Store_double_flat_field(accu, *pc, Double_val(*sp));
      accu = Val_unit;
      sp++;
      pc++;
      Next;

/* Array operations */

    Instruct(VECTLENGTH): {
      /* Todo: when FLAT_FLOAT_ARRAY is false, this instruction should
         be split into VECTLENGTH and FLOATVECTLENGTH because we know
         statically which one it is. */
      mlsize_t size = Wosize_val(accu);
      if (Tag_val(accu) == Double_array_tag) size = size / Double_wosize;
      accu = Val_long(size);
      Next;
    }
    Instruct(GETVECTITEM):
      Accu_field(Long_val(sp[0]));
      sp += 1;
      Next;
    Instruct(SETVECTITEM):
      caml_modify_field(accu, Long_val(sp[0]), sp[1]);
      accu = Val_unit;
      sp += 2;
      Next;

/* String operations */

    Instruct(GETSTRINGCHAR):
      accu = Val_int(Byte_u(accu, Long_val(sp[0])));
      sp += 1;
      Next;
    Instruct(SETSTRINGCHAR):
      Byte_u(accu, Long_val(sp[0])) = Int_val(sp[1]);
      sp += 2;
      accu = Val_unit;
      Next;

/* Branches and conditional branches */

    Instruct(BRANCH):
      pc += *pc;
      Next;
    Instruct(BRANCHIF):
      if (accu != Val_false) pc += *pc; else pc++;
      Next;
    Instruct(BRANCHIFNOT):
      if (accu == Val_false) pc += *pc; else pc++;
      Next;
    Instruct(SWITCH): {
      uint32_t sizes = *pc++;
      if (Is_block(accu)) {
        intnat index = Tag_val(accu);
        CAMLassert ((uintnat) index < (sizes >> 16));
        pc += pc[(sizes & 0xFFFF) + index];
      } else {
        intnat index = Long_val(accu);
        CAMLassert ((uintnat) index < (sizes & 0xFFFF)) ;
        pc += pc[index];
      }
      Next;
    }
    Instruct(BOOLNOT):
      accu = Val_not(accu);
      Next;

/* Exceptions */

    Instruct(PUSHTRAP):
      sp -= 4;
      Trap_pc(sp) = Val_pc(pc + *pc);
      Trap_link(sp) = Val_long(domain_state->trap_sp_off);
      sp[2] = env;
      sp[3] = Val_long(extra_args);
      domain_state->trap_sp_off = sp - Stack_high(domain_state->current_stack);
      pc++;
      Next;

    Instruct(POPTRAP):
      if (Caml_check_gc_interrupt(domain_state)) {
        /* We must check here so that if a signal is pending and its
           handler triggers an exception, the exception is trapped
           by the current try...with, not the enclosing one. */
        pc--; /* restart the POPTRAP after processing the signal */
        goto process_signal;
      }
      domain_state->trap_sp_off = Long_val(Trap_link(sp));
      sp += 4;
      Next;

    Instruct(RAISE_NOTRACE):
      if (domain_state->trap_sp_off >= domain_state->trap_barrier_off)
        caml_debugger(TRAP_BARRIER);
      goto raise_notrace;

    Instruct(RERAISE):
      if (domain_state->trap_sp_off >= domain_state->trap_barrier_off) caml_debugger(TRAP_BARRIER);
      if (domain_state->backtrace_active) caml_stash_backtrace(accu, pc, sp, 1);
      goto raise_notrace;

    Instruct(RAISE):
    raise_exception:
      if (domain_state->trap_sp_off >= domain_state->trap_barrier_off) caml_debugger(TRAP_BARRIER);
      if (domain_state->backtrace_active) caml_stash_backtrace(accu, pc, sp, 0);
    raise_notrace:
      if (domain_state->trap_sp_off > 0) {
        if (Stack_parent(domain_state->current_stack) == NULL) {
          domain_state->external_raise = initial_external_raise;
          domain_state->trap_sp_off = initial_trap_sp_off;
          domain_state->current_stack->sp =
            Stack_high(domain_state->current_stack) - initial_stack_words ;
          caml_decr_callback_depth ();
          return Make_exception_result(accu);
        } else {
          struct stack_info* parent_stack =
            Stack_parent(domain_state->current_stack);
          value hexn = Stack_handle_exception(domain_state->current_stack);
          domain_state->current_stack->sp = sp;
          struct stack_info* old_stack = caml_switch_stack(parent_stack);
          sp = domain_state->current_stack->sp;
          caml_free_stack(old_stack);

          domain_state->trap_sp_off = Long_val(sp[0]);
          extra_args = Long_val(sp[1]);
          sp++;
          sp[0] = accu;

          accu = hexn;
          pc = Code_val(accu);
          env = accu;
          goto check_stacks;
        }
      } else {
        sp = Stack_high(domain_state->current_stack) + domain_state->trap_sp_off;
        pc = Pc_val(Trap_pc(sp));
        domain_state->trap_sp_off = Long_val(Trap_link(sp));
        env = sp[2];
        extra_args = Long_val(sp[3]);
        sp += 4;
      }
      Next;



/* Stack checks */

    check_stacks:
      if (sp < Stack_threshold_ptr(domain_state->current_stack)) {
        domain_state->current_stack->sp = sp;
        if (!caml_try_realloc_stack(Stack_threshold / sizeof(value))) {
          Setup_for_c_call; caml_raise_stack_overflow();
        }
        sp = domain_state->current_stack->sp;
      }
      /* Fall through CHECK_SIGNALS */

/* Signal handling */

    Instruct(CHECK_SIGNALS):    /* accu not preserved */
      if (Caml_check_gc_interrupt(domain_state)) goto process_signal;
      Next;

    process_signal:
      Setup_for_event;
      caml_handle_gc_interrupt();
      Restore_after_event;
      Next;

/* Calling C functions */

    Instruct(C_CALL1):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu);
      Restore_after_c_call;
      pc++;
      Next;
    Instruct(C_CALL2):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu, sp[1]);
      Restore_after_c_call;
      sp += 1;
      pc++;
      Next;
    Instruct(C_CALL3):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu, sp[1], sp[2]);
      Restore_after_c_call;
      sp += 2;
      pc++;
      Next;
    Instruct(C_CALL4):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu, sp[1], sp[2], sp[3]);
      Restore_after_c_call;
      sp += 3;
      pc++;
      Next;
    Instruct(C_CALL5):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu, sp[1], sp[2], sp[3], sp[4]);
      Restore_after_c_call;
      sp += 4;
      pc++;
      Next;
    Instruct(C_CALLN): {
      int nargs = *pc++;
      *--sp = accu;
      Setup_for_c_call;
      accu = Primitive(*pc)(sp + 1, nargs);
      Restore_after_c_call;
      sp += nargs;
      pc++;
      Next;
    }

/* Integer constants */

    Instruct(CONST0):
      accu = Val_int(0); Next;
    Instruct(CONST1):
      accu = Val_int(1); Next;
    Instruct(CONST2):
      accu = Val_int(2); Next;
    Instruct(CONST3):
      accu = Val_int(3); Next;

    Instruct(PUSHCONST0):
      *--sp = accu; accu = Val_int(0); Next;
    Instruct(PUSHCONST1):
      *--sp = accu; accu = Val_int(1); Next;
    Instruct(PUSHCONST2):
      *--sp = accu; accu = Val_int(2); Next;
    Instruct(PUSHCONST3):
      *--sp = accu; accu = Val_int(3); Next;

    Instruct(PUSHCONSTINT):
      *--sp = accu;
      /* Fallthrough */
    Instruct(CONSTINT):
      accu = Val_int(*pc);
      pc++;
      Next;

/* Integer arithmetic */

    Instruct(NEGINT):
      accu = (value)(2 - (intnat)accu); Next;
    Instruct(ADDINT):
      accu = (value)((intnat) accu + (intnat) *sp++ - 1); Next;
    Instruct(SUBINT):
      accu = (value)((intnat) accu - (intnat) *sp++ + 1); Next;
    Instruct(MULINT):
      accu = Val_long(Long_val(accu) * Long_val(*sp++)); Next;

    Instruct(DIVINT): {
      intnat divisor = Long_val(*sp++);
      if (divisor == 0) { Setup_for_c_call; caml_raise_zero_divide(); }
      accu = Val_long(Long_val(accu) / divisor);
      Next;
    }
    Instruct(MODINT): {
      intnat divisor = Long_val(*sp++);
      if (divisor == 0) { Setup_for_c_call; caml_raise_zero_divide(); }
      accu = Val_long(Long_val(accu) % divisor);
      Next;
    }
    Instruct(ANDINT):
      accu = (value)((intnat) accu & (intnat) *sp++); Next;
    Instruct(ORINT):
      accu = (value)((intnat) accu | (intnat) *sp++); Next;
    Instruct(XORINT):
      accu = (value)(((intnat) accu ^ (intnat) *sp++) | 1); Next;
    Instruct(LSLINT):
      accu = (value)((((intnat) accu - 1) << Long_val(*sp++)) + 1); Next;
    Instruct(LSRINT):
      accu = (value)((((uintnat) accu - 1) >> Long_val(*sp++)) | 1);
      Next;
    Instruct(ASRINT):
      accu = (value)((((intnat) accu - 1) >> Long_val(*sp++)) | 1); Next;

#define Integer_comparison(typ,opname,tst) \
    Instruct(opname): \
      accu = Val_int((typ) accu tst (typ) *sp++); Next;

    Integer_comparison(intnat,EQ, ==)
    Integer_comparison(intnat,NEQ, !=)
    Integer_comparison(intnat,LTINT, <)
    Integer_comparison(intnat,LEINT, <=)
    Integer_comparison(intnat,GTINT, >)
    Integer_comparison(intnat,GEINT, >=)
    Integer_comparison(uintnat,ULTINT, <)
    Integer_comparison(uintnat,UGEINT, >=)

#define Integer_branch_comparison(typ,opname,tst,debug) \
    Instruct(opname): \
      if ( *pc++ tst (typ) Long_val(accu)) { \
        pc += *pc ; \
      } else { \
        pc++ ; \
      } ; Next;

    Integer_branch_comparison(intnat,BEQ, ==, "==")
    Integer_branch_comparison(intnat,BNEQ, !=, "!=")
    Integer_branch_comparison(intnat,BLTINT, <, "<")
    Integer_branch_comparison(intnat,BLEINT, <=, "<=")
    Integer_branch_comparison(intnat,BGTINT, >, ">")
    Integer_branch_comparison(intnat,BGEINT, >=, ">=")
    Integer_branch_comparison(uintnat,BULTINT, <, "<")
    Integer_branch_comparison(uintnat,BUGEINT, >=, ">=")

    Instruct(OFFSETINT):
      accu += *pc << 1;
      pc++;
      Next;
    Instruct(OFFSETREF): {
        long n = Long_field(accu, 0);
        n += *pc;
        caml_modify_field(accu, 0, Val_long(n));
      }
      accu = Val_unit;
      pc++;
      Next;
    Instruct(ISINT):
      accu = Val_long(accu & 1);
      Next;

/* Object-oriented operations */

      /* please don't forget to keep below code in sync with the
         functions caml_cache_public_method and
         caml_cache_public_method2 in obj.c */

    Instruct(GETMETHOD):
      accu = Field_imm (Field_imm(sp[0], 0), Int_val(accu));
      Next;

#define CAML_METHOD_CACHE
#ifdef CAML_METHOD_CACHE
    Instruct(GETPUBMET): {
      /* accu == object, pc[0] == tag, pc[1] == cache */
      value meths = Field_imm (accu, 0);
      value ofs;
#ifdef CAML_TEST_CACHE
      static int calls = 0, hits = 0;
      if (calls >= 10000000) {
        fprintf(stderr, "cache hit = %d%%\n", hits / 100000);
        calls = 0; hits = 0;
      }
      calls++;
#endif
      *--sp = accu;
      accu = Val_int(*pc++);
      ofs = *pc & Field_imm(meths,1);
      if (*(value*)(((char*)(Op_val(meths)+3)) + ofs) == accu) {
#ifdef CAML_TEST_CACHE
        hits++;
#endif
        accu = *(value*)(((char*)(Op_val(meths)+2)) + ofs);
      }
      else
      {
        int li = 3, hi = Field_imm(meths,0), mi;
        while (li < hi) {
          mi = ((li+hi) >> 1) | 1;
          if (accu < Field_imm(meths,mi)) hi = mi-2;
          else li = mi;
        }
        *pc = (li-3)*sizeof(value);
        accu = Field_imm (meths, li-1);
      }
      pc++;
      Next;
    }
#else
    Instruct(GETPUBMET):
      *--sp = accu;
      accu = Val_int(*pc);
      pc += 2;
      /* Fallthrough */
#endif
    Instruct(GETDYNMET): {
      /* accu == tag, sp[0] == object, *pc == cache */
      value meths = Field_imm (sp[0], 0);
      int li = 3, hi = Field_imm (meths,0), mi;
      while (li < hi) {
        mi = ((li+hi) >> 1) | 1;
        if (accu < Field_imm (meths,mi)) hi = mi-2;
        else li = mi;
      }
      accu = Field_imm (meths, li-1);
      Next;
    }

/* Debugging and machine control */

    Instruct(STOP):
      domain_state->external_raise = initial_external_raise;
      domain_state->trap_sp_off = initial_trap_sp_off;
      domain_state->current_stack->sp = sp;
      caml_decr_callback_depth ();
      return accu;

    Instruct(EVENT):
      if (--caml_event_count == 0) {
        Setup_for_debugger;
        caml_debugger(EVENT_COUNT);
        Restore_after_debugger;
      }
      Restart_curr_instr;

    Instruct(BREAK):
      Setup_for_debugger;
      caml_debugger(BREAKPOINT);
      Restore_after_debugger;
      Restart_curr_instr;

/* Context switching */
      value resume_fn, resume_arg;

    Instruct(RESUME):
      resume_fn = sp[0];
      resume_arg = sp[1];
      sp -= 3;
      sp[0] = Val_long(domain_state->trap_sp_off);
      sp[1] = Val_long(0);
      sp[2] = Val_pc(pc);
      sp[3] = env;
      sp[4] = Val_long(extra_args);
      goto do_resume;

do_resume: {
      struct stack_info* stk = Ptr_val(accu);
      while (Stack_parent(stk) != NULL) stk = Stack_parent(stk);
      Stack_parent(stk) = Caml_state->current_stack;

      domain_state->current_stack->sp = sp;
      caml_switch_stack(Ptr_val(accu));
      sp = domain_state->current_stack->sp;

      domain_state->trap_sp_off = Long_val(sp[0]);
      sp[0] = resume_arg;
      accu = resume_fn;
      pc = Code_val(accu);
      env = accu;
      extra_args = 0;
      goto check_stacks;
    }

    Instruct(RESUMETERM):
      resume_fn = sp[0];
      resume_arg = sp[1];
      sp = sp + *pc - 2;
      sp[0] = Val_long(domain_state->trap_sp_off);
      sp[1] = Val_long(extra_args);
      goto do_resume;


    Instruct(PERFORM): {
      value cont;
      struct stack_info* parent_stack =
        Stack_parent(domain_state->current_stack);

      if (parent_stack == NULL) {
        accu = Field_imm(caml_read_root(caml_global_data), UNHANDLED_EXN);
        goto raise_exception;
      }

      Alloc_small(cont, 1, Cont_tag, Enter_gc);

      sp -= 4;
      sp[0] = Val_long(domain_state->trap_sp_off);
      sp[1] = Val_pc(pc);
      sp[2] = env;
      sp[3] = Val_long(extra_args);

      domain_state->current_stack->sp = sp;
      struct stack_info* old_stack = caml_switch_stack(parent_stack);
      sp = domain_state->current_stack->sp;
      Stack_parent(old_stack) = NULL;
      Init_field(cont, 0, Val_ptr(old_stack));

      domain_state->trap_sp_off = Long_val(sp[0]);
      extra_args = Long_val(sp[1]);
      sp--;
      sp[0] = accu;
      sp[1] = cont;
      sp[2] = Val_ptr(old_stack);
      accu = Stack_handle_effect(old_stack);
      pc = Code_val(accu);
      env = accu;
      extra_args += 2;
      goto check_stacks;
    }

    Instruct(REPERFORMTERM): {
      value eff = accu;
      value cont = sp[0];
      struct stack_info* cont_tail = Ptr_val(sp[1]);
      struct stack_info* self;
      struct stack_info* parent = Stack_parent(domain_state->current_stack);

      sp = sp + *pc - 2;
      sp[0] = Val_long(domain_state->trap_sp_off);
      sp[1] = Val_long(extra_args);

      if (parent == NULL) {
        accu = caml_continuation_use(cont);
        resume_fn = caml_read_root(raise_unhandled);
        resume_arg = Field_imm(caml_read_root(caml_global_data), UNHANDLED_EXN);
        goto do_resume;
      }

      domain_state->current_stack->sp = sp;
      self = caml_switch_stack(parent);
      sp = domain_state->current_stack->sp;

      CAMLassert(Stack_parent(cont_tail) == NULL);
      Stack_parent(self) = NULL;
      Stack_parent(cont_tail) = self;

      domain_state->trap_sp_off = Long_val(sp[0]);
      extra_args = Long_val(sp[1]);
      sp--;
      sp[0] = eff;
      sp[1] = cont;
      sp[2] = Val_ptr(self);
      accu = Stack_handle_effect(self);
      pc = Code_val(accu);
      env = accu;
      extra_args += 2;
      goto check_stacks;
    }

#ifndef THREADED_CODE
    default:
#if _MSC_VER >= 1200
      __assume(0);
#else
      caml_fatal_error_arg("Fatal error: bad opcode (%"
                           ARCH_INTNAT_PRINTF_FORMAT "x)\n",
                           (char *) (intnat) *(pc-1));
#endif
    }
  }
#endif
}
