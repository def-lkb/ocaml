/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2006 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Stack backtrace for uncaught exceptions */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "stack.h"

int caml_backtrace_active = 0;
int caml_backtrace_pos = 0;
code_t * caml_backtrace_buffer = NULL;
value caml_backtrace_last_exn = Val_unit;
#define BACKTRACE_BUFFER_SIZE 1024

/* In order to prevent the GC from walking through the debug information
   (which have no headers), we transform frame_descr pointers into
   31/63 bits ocaml integers by shifting them by 1 to the right. We do
   not lose information as descr pointers are aligned.

   In particular, we do not need to use [caml_initialize] when setting
   an array element with such a value.
*/
#define Val_Debuginfo(descr) Val_long((uintnat)descr>>1 & ~1)
#define Debuginfo_val(v) ((frame_descr *) (Long_val(v)<<1 & ~1))

/* Start or stop the backtrace machinery */

CAMLprim value caml_record_backtrace(value vflag)
{
  int flag = Int_val(vflag);

  if (flag != caml_backtrace_active) {
    caml_backtrace_active = flag;
    caml_backtrace_pos = 0;
    if (flag) {
      caml_register_global_root(&caml_backtrace_last_exn);
    } else {
      caml_remove_global_root(&caml_backtrace_last_exn);
    }
  }
  return Val_unit;
}

/* Return the status of the backtrace machinery */

CAMLprim value caml_backtrace_status(value vunit)
{
  return Val_bool(caml_backtrace_active);
}

/* returns the next frame descriptor (or NULL if none is available),
   and updates *pc and *sp to point to the following one.  */

frame_descr * caml_next_frame_descriptor(uintnat * pc, char ** sp)
{
  frame_descr * d;
  uintnat h;

  if (caml_frame_descriptors == NULL) caml_init_frame_descriptors();

  while (1) {
    h = Hash_retaddr(*pc);
    while (1) {
      d = caml_frame_descriptors[h];
      if (d == 0) return NULL; /* can happen if some code compiled without -g */
      if (d->retaddr == *pc) break;
      h = (h+1) & caml_frame_descriptors_mask;
    }
    /* Skip to next frame */
    if (d->frame_size != 0xFFFF) {
      /* Regular frame, update sp/pc and return the frame descriptor */
#ifndef Stack_grows_upwards
      *sp += (d->frame_size & 0xFFFC);
#else
      *sp -= (d->frame_size & 0xFFFC);
#endif
      *pc = Saved_return_address(*sp);
#ifdef Mask_already_scanned
      *pc = Mask_already_scanned(*pc);
#endif
      return d;
    } else {
      /* Special frame marking the top of a stack chunk for an ML callback.
         Skip C portion of stack and continue with next ML stack chunk. */
      struct caml_context * next_context = Callback_link(*sp);
      *sp = next_context->bottom_of_stack;
      *pc = next_context->last_retaddr;
      /* A null sp means no more ML stack chunks; stop here. */
      if (*sp == NULL) return NULL;
    }
  }
}

/* Extract location information for the given frame descriptor */

static void *extract_debuginfo(frame_descr * d)
{
  uintnat debuginfo;
  if ((d->frame_size & 1) == 0) {
    return NULL;
  }

  debuginfo = ((uintnat) d +
               sizeof(char *) + sizeof(short) + sizeof(short) +
               sizeof(short) * d->num_live + sizeof(frame_descr *) - 1)
              & -sizeof(frame_descr *);

  return *(void**)debuginfo;
}

static void *extract_debuginfo_next(void *debuginfo)
{
  uint32 *infoptr = debuginfo;

  if (debuginfo == NULL)
    return NULL;

  return *(void**)(infoptr + 2);
}

static void unpack_location_info(void * debuginfo, /*out*/ struct caml_loc_info * li)
{
  uint32 *infoptr = debuginfo;
  uint32 info1, info2;

  if (infoptr == NULL) {
    li->loc_valid = 0;
    li->loc_is_raise = 1;
    li->loc_is_inlined = 0;
    return;
  }

  info1 = infoptr[0];
  info2 = infoptr[1];

  /* Format of the two info words:
       llllllllllllllllllll aaaaaaaa bbbbbbbbbb nnnnnnnnnnnnnnnnnnnnnnnn i k
                          44       36         26                       2 1 0
                       (32+12)    (32+4)
     k ( 1 bit ): 0 if it's a call, 1 if it's a raise
     i ( 1 bit ): 1 if the site is inlined
     n (24 bits):
      if i == 0, offset (in 4-byte words) of file name relative to infoptr
      else if i == 1, offset (in 4-byte words) of chain entry relative to infoptr
     l (20 bits): line number
     a ( 8 bits): beginning of character range
     b (10 bits): end of character range */
  li->loc_valid = 1;
  li->loc_is_raise = (info1 & 1) != 0;
  li->loc_is_inlined = (info1 & 2) != 0;
  li->loc_lnum = info2 >> 12;
  li->loc_startchr = (info2 >> 4) & 0xFF;
  li->loc_endchr = ((info2 & 0xF) << 6) | (info1 >> 26);
  li->loc_filename = (char *)infoptr + (info1 & 0x3FFFFFC);
}

CAMLexport void extract_location_info(frame_descr * d,
                                  /*out*/ struct caml_loc_info * li)
{
  unpack_location_info(extract_debuginfo(d), li);
}

static int count_entries(frame_descr * d)
{
  int i = 0;
  void * iter;

  for (iter = extract_debuginfo(d); iter != NULL; iter = extract_debuginfo_next(iter))
    i += 1;

  if (i == 0)
    return 1; /* One entry for "compiler inserted reraise" */
  else
    return i;
}

/* Stores the return addresses contained in the given stack fragment
   into the backtrace array ; this version is performance-sensitive as
   it is called at each [raise] in a program compiled with [-g], so we
   preserved the global, statically bounded buffer of the old
   implementation -- before the more flexible
   [caml_get_current_callstack] was implemented. */

void caml_stash_backtrace(value exn, uintnat pc, char * sp, char * trapsp)
{
  if (exn != caml_backtrace_last_exn) {
    caml_backtrace_pos = 0;
    caml_backtrace_last_exn = exn;
  }
  if (caml_backtrace_buffer == NULL) {
    Assert(caml_backtrace_pos == 0);
    caml_backtrace_buffer = malloc(BACKTRACE_BUFFER_SIZE * sizeof(code_t));
    if (caml_backtrace_buffer == NULL) return;
  }

  /* iterate on each frame  */
  while (1) {
    frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
    if (descr == NULL) return;
    /* store its descriptor in the backtrace buffer */
    if (caml_backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;
    caml_backtrace_buffer[caml_backtrace_pos++] = (code_t) descr;

    /* Stop when we reach the current exception handler */
#ifndef Stack_grows_upwards
    if (sp > trapsp) return;
#else
    if (sp < trapsp) return;
#endif
  }
}

/* Stores upto [max_frames_value] frames of the current call stack to
   return to the user. This is used not in an exception-raising
   context, but only when the user requests to save the trace
   (hopefully less often). Instead of using a bounded buffer as
   [caml_stash_backtrace], we first traverse the stack to compute the
   right size, then allocate space for the trace. */

CAMLprim value caml_get_current_callstack(value max_frames_value) {
  CAMLparam1(max_frames_value);
  CAMLlocal1(trace);

  /* we use `intnat` here because, were it only `int`, passing `max_int`
     from the OCaml side would overflow on 64bits machines. */
  intnat max_frames = Long_val(max_frames_value);
  intnat trace_size;

  /* first compute the size of the trace */
  {
    uintnat pc = caml_last_return_address;
    /* note that [caml_bottom_of_stack] always points to the most recent
     * frame, independently of the [Stack_grows_upwards] setting */
    char * sp = caml_bottom_of_stack;
    char * limitsp = caml_top_of_stack;

    trace_size = 0;
    while (1) {
      frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
      if (descr == NULL) break;
      if (trace_size >= max_frames) break;
      trace_size += count_entries(descr);

#ifndef Stack_grows_upwards
      if (sp > limitsp) break;
#else
      if (sp < limitsp) break;
#endif
    }
  }

  if (trace_size >= max_frames)
    trace_size = max_frames;

  trace = caml_alloc((mlsize_t) trace_size, 0);

  /* then collect the trace */
  {
    uintnat pc = caml_last_return_address;
    char * sp = caml_bottom_of_stack;
    intnat trace_pos;
    void * debuginfo = NULL;

    for (trace_pos = 0; trace_pos < trace_size; trace_pos++) {
      if (debuginfo != NULL) {
        /* Inlined entry */
        Field(trace, trace_pos) = Val_Debuginfo(debuginfo);
        debuginfo = extract_debuginfo_next(debuginfo);

      } else {
        /* Normal entry */
        frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
        Assert(descr != NULL);

        debuginfo = extract_debuginfo(descr);
        Field(trace, trace_pos) = Val_Debuginfo(debuginfo);

        debuginfo = extract_debuginfo_next(debuginfo);
      }
    }
  }

  CAMLreturn(trace);
}

/* Print location information -- same behavior as in Printexc

   note that the test for compiler-inserted raises is slightly redundant:
     (!li->loc_valid && li->loc_is_raise)
   extract_location_info above guarantees that when li->loc_valid is
   0, then li->loc_is_raise is always 1, so the latter test is
   useless. We kept it to keep code identical to the byterun/
   implementation. */

static void print_location(struct caml_loc_info * li, int index)
{
  char * inlined;
  char * info;

  /* Ignore compiler-inserted raise */
  if (!li->loc_valid && li->loc_is_raise) return;

  if (li->loc_is_raise) {
    /* Initial raise if index == 0, re-raise otherwise */
    if (index == 0)
      info = "Raised at";
    else
      info = "Re-raised at";
  } else {
    if (index == 0)
      info = "Raised by primitive operation at";
    else
      info = "Called from";
  }

  if (li->loc_is_inlined) {
    inlined = " (inlined)";
  } else {
    inlined = "";
  }

  if (! li->loc_valid) {
    fprintf(stderr, "%s unknown location%s\n", info, inlined);
  } else {
    fprintf (stderr, "%s file \"%s\"%s, line %d, characters %d-%d\n",
             info, li->loc_filename, inlined,
             li->loc_lnum, li->loc_startchr, li->loc_endchr);
  }
}

/* Print a backtrace */

void caml_print_exception_backtrace(void)
{
  int i, index;
  struct caml_loc_info li;

  for (i = 0, index = 0; i < caml_backtrace_pos; i++) {
    void * debuginfo = extract_debuginfo((frame_descr*)(caml_backtrace_buffer[i]));
    unpack_location_info(debuginfo, &li);
    print_location(&li, index);

    for (debuginfo = extract_debuginfo_next(debuginfo);
         debuginfo != NULL;
         debuginfo = extract_debuginfo_next(debuginfo))
    {
      unpack_location_info(debuginfo, &li);
      print_location(&li, index);
      index++;
    }
  }
}

/* Convert the raw backtrace to a data structure usable from OCaml */

CAMLprim value caml_convert_raw_backtrace_slot(value backtrace_slot) {
  CAMLparam1(backtrace_slot);
  CAMLlocal2(p, fname);
  struct caml_loc_info li;

  unpack_location_info(Debuginfo_val(backtrace_slot), &li);

  if (li.loc_valid) {
    fname = caml_copy_string(li.loc_filename);
    p = caml_alloc_small(6, 0);
    Field(p, 0) = Val_bool(li.loc_is_raise);
    Field(p, 1) = fname;
    Field(p, 2) = Val_int(li.loc_lnum);
    Field(p, 3) = Val_int(li.loc_startchr);
    Field(p, 4) = Val_int(li.loc_endchr);
    Field(p, 5) = Val_int(li.loc_is_inlined);
  } else {
    p = caml_alloc_small(1, 1);
    Field(p, 0) = Val_bool(li.loc_is_raise);
  }

  CAMLreturn(p);
}

/* Get a copy of the latest backtrace */

CAMLprim value caml_get_exception_raw_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal1(res);
  const int tag = 0;

  /* Beware: the allocations below may cause finalizers to be run, and another
     backtrace---possibly of a different length---to be stashed (for example
     if the finalizer raises then catches an exception).  We choose to ignore
     any such finalizer backtraces and return the original one. */

  if (caml_backtrace_buffer == NULL || caml_backtrace_pos == 0) {
    res = caml_alloc(0, tag);
  }
  else {
    code_t saved_caml_backtrace_buffer[BACKTRACE_BUFFER_SIZE];
    int saved_caml_backtrace_pos;
    int backtrace_size;
    intnat i, j;

    saved_caml_backtrace_pos = caml_backtrace_pos;

    if (saved_caml_backtrace_pos > BACKTRACE_BUFFER_SIZE) {
      saved_caml_backtrace_pos = BACKTRACE_BUFFER_SIZE;
    }

    memcpy(saved_caml_backtrace_buffer, caml_backtrace_buffer,
           saved_caml_backtrace_pos * sizeof(code_t));

    backtrace_size = saved_caml_backtrace_pos;
    for (i = 0; i < saved_caml_backtrace_pos; i++) {
      frame_descr * descr = (frame_descr *) (saved_caml_backtrace_buffer[i]);
      backtrace_size += count_entries(descr);
    }

    res = caml_alloc(backtrace_size, tag);
    for (i = 0, j = 0; i < saved_caml_backtrace_pos; i++, j++) {
      frame_descr * descr = (frame_descr *) (saved_caml_backtrace_buffer[i]);
      void * debuginfo = extract_debuginfo(descr);

      /* [Val_Debuginfo] always returns an immediate. */
      Field(res, j) = Val_Debuginfo(debuginfo);

      for (debuginfo = extract_debuginfo_next(debuginfo);
           debuginfo != NULL;
           debuginfo = extract_debuginfo_next(debuginfo))
      {
        Field(res, j) = Val_Debuginfo(debuginfo);
        j += 1;
      }
    }
  }

  CAMLreturn(res);
}

/* the function below is deprecated: we previously returned directly
   the OCaml-usable representation, instead of the raw backtrace as an
   abstract type, but this has a large performance overhead if you
   store a lot of backtraces and print only some of them.

   It is not used by the Printexc library anymore, or anywhere else in
   the compiler, but we have kept it in case some user still depends
   on it as an external.
*/

CAMLprim value caml_get_exception_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal3(arr, res, backtrace);
  intnat i;

  backtrace = caml_get_exception_raw_backtrace(Val_unit);

  arr = caml_alloc(Wosize_val(backtrace), 0);
  for (i = 0; i < Wosize_val(backtrace); i++) {
    Store_field(arr, i, caml_convert_raw_backtrace_slot(Field(backtrace, i)));
  }

  res = caml_alloc_small(1, 0); Field(res, 0) = arr; /* Some */
  CAMLreturn(res);
}

/* Turn encoded retaddr into a raw_backtrace_slot */
CAMLprim value caml_caller_slot(value retaddr)
{
  CAMLparam1(retaddr);
  CAMLlocal2(result, slot);

  frame_descr *d;
  uintnat h;

  if (caml_frame_descriptors == NULL) caml_init_frame_descriptors();

  if (Is_long(retaddr) && retaddr > 1) {
    /* Find the descriptor corresponding to the return address */
    h = Hash_retaddr(retaddr);
    while (1) {
      d = caml_frame_descriptors[h];
      if (d == NULL) break;

      /* The | 1 is a hack to store retaddr as a ocaml int,
       * but we don't expect two frame descriptors to be spaced by less than 1 */
      if ((d->retaddr | 1) == retaddr) break;
      h = (h+1) & caml_frame_descriptors_mask;
    }

    if (d != NULL) {
      slot = Val_Debuginfo(d);
      result = caml_alloc_small(1, 0);
      Field(result, 0) = slot;
    } else {
      result = Val_unit;
    }
  } else {
    result = Val_unit;
  }

  CAMLreturn(result);
}
