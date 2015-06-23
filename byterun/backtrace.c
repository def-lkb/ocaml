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
#include "caml/memory.h"
#include "caml/backtrace.h"
#include "caml/backtrace_prim.h"

CAMLexport int caml_backtrace_active = 0;
CAMLexport int caml_backtrace_pos = 0;
CAMLexport backtrace_slot * caml_backtrace_buffer = NULL;
CAMLexport value caml_backtrace_last_exn = Val_unit;

void caml_init_backtrace(void)
{
  caml_register_global_root(&caml_backtrace_last_exn);
}

/* Start or stop the backtrace machinery */
CAMLprim value caml_record_backtrace(value vflag)
{
  int flag = Int_val(vflag);

  if (flag != caml_backtrace_active) {
    caml_backtrace_active = flag;
    caml_backtrace_pos = 0;
    caml_backtrace_last_exn = Val_unit;
    /* Note: lazy initialization of caml_backtrace_buffer in
       caml_stash_backtrace to simplify the interface with the thread
       libraries */
  }
  return Val_unit;
}

/* Return the status of the backtrace machinery */
CAMLprim value caml_backtrace_status(value vunit)
{
  return Val_bool(caml_backtrace_active);
}

/* Print location information -- same behavior as in Printexc

   note that the test for compiler-inserted raises is slightly redundant:
     (!li->loc_valid && li->loc_is_raise)
   caml_extract_location_info above guarantees that when li->loc_valid is
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
CAMLexport void caml_print_exception_backtrace(void)
{
  int i, index;
  struct caml_loc_info li;

  if (!caml_debug_info_available()) {
    fprintf(stderr, "(Cannot print stack backtrace: no debug information available)\n");
    return;
  }

  for (i = 0, index = 0; i < caml_backtrace_pos; i++) {
    backtrace_slot slot = caml_backtrace_buffer[i];
    do {
      caml_extract_location_info(slot, &li);
      print_location(&li, index);
      index += 1;
      slot = caml_backtrace_next_inlined(slot);
    } while (Is_backtrace_slot_valid(slot));
  }
}

/* Get a copy of the latest backtrace */
CAMLprim value caml_get_exception_raw_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal1(res);

  /* Beware: the allocations below may cause finalizers to be run, and another
     backtrace---possibly of a different length---to be stashed (for example
     if the finalizer raises then catches an exception).  We choose to ignore
     any such finalizer backtraces and return the original one. */

  if (!caml_backtrace_active ||
      caml_backtrace_buffer == NULL ||
      caml_backtrace_pos == 0) {
    res = caml_alloc(0, 0);
  }
  else {
    backtrace_slot saved_caml_backtrace_buffer[BACKTRACE_BUFFER_SIZE];
    int saved_caml_backtrace_pos, words;
    intnat i, index;

    saved_caml_backtrace_pos = caml_backtrace_pos;

    if (saved_caml_backtrace_pos > BACKTRACE_BUFFER_SIZE) {
      saved_caml_backtrace_pos = BACKTRACE_BUFFER_SIZE;
    }

    for (i = 0, words = saved_caml_backtrace_pos; i < saved_caml_backtrace_pos; ++i) {
      backtrace_slot slot = caml_backtrace_buffer[i];
      saved_caml_backtrace_buffer[i] = slot;
      words += caml_backtrace_count_inlined(slot);
    }

    res = caml_alloc(words, 0);
    for (i = 0, index = 0; i < saved_caml_backtrace_pos; ++i) {
      backtrace_slot slot = saved_caml_backtrace_buffer[i];
      do {
        Store_field(res, index, caml_val_raw_backtrace_slot(slot));
        index += 1;
        slot = caml_backtrace_next_inlined(slot);
      } while (Is_backtrace_slot_valid(slot));
    }
  }

  CAMLreturn(res);
}

/* Convert the raw backtrace to a data structure usable from OCaml */
CAMLprim value caml_convert_raw_backtrace_slot(value backtrace_slot)
{
  CAMLparam1(backtrace_slot);
  CAMLlocal2(p, fname);
  struct caml_loc_info li;

  if (!caml_debug_info_available())
    caml_failwith("No debug information available");

  caml_extract_location_info(caml_raw_backtrace_slot_val(backtrace_slot), &li);

  if (li.loc_valid) {
    fname = caml_copy_string(li.loc_filename);
    p = caml_alloc_small(6, 0);
    Field(p, 0) = Val_bool(li.loc_is_raise);
    Field(p, 1) = fname;
    Field(p, 2) = Val_int(li.loc_lnum);
    Field(p, 3) = Val_int(li.loc_startchr);
    Field(p, 4) = Val_int(li.loc_endchr);
    Field(p, 5) = Val_bool(li.loc_is_inlined);
  } else {
    p = caml_alloc_small(1, 1);
    Field(p, 0) = Val_bool(li.loc_is_raise);
  }

  CAMLreturn(p);
}

/* the function below is deprecated: we previously returned directly
   the OCaml-usable representation, instead of the raw backtrace as an
   abstract type, but this has a large performance overhead if you
   store a lot of backtraces and print only some of them.

   It is not used by the Printexc library anymore, or anywhere else in
   the compiler, but we have kept it in case some user still depends
   on it as an external.  */
CAMLprim value caml_get_exception_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal3(arr, res, backtrace);
  intnat i;

  if (!caml_debug_info_available()) {
    res = Val_unit; /* None */
  } else {
    backtrace = caml_get_exception_raw_backtrace(Val_unit);

    arr = caml_alloc(Wosize_val(backtrace), 0);
    for (i = 0; i < Wosize_val(backtrace); i++) {
      Store_field(arr, i, caml_convert_raw_backtrace_slot(Field(backtrace, i)));
    }

    res = caml_alloc_small(1, 0); Field(res, 0) = arr; /* Some */
  }
  CAMLreturn(res);
}
