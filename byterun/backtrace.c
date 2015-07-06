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
#include "caml/printexc.h"

CAMLexport int caml_backtrace_active = 0;
CAMLexport int caml_backtrace_pos = 0;
CAMLexport backtrace_slot *caml_backtrace_buffer = NULL;
CAMLexport value caml_backtrace_exns_values = Val_unit;
CAMLexport int *caml_backtrace_exns_positions = NULL;
CAMLexport int caml_backtrace_exns_index = 0;

void caml_init_backtrace(void)
{
  caml_register_global_root(&caml_backtrace_exns_values);
  caml_backtrace_exns_values = caml_alloc(BACKTRACE_EXN_COUNT, 0);
  caml_backtrace_exns_positions = malloc(sizeof(int) * BACKTRACE_EXN_COUNT);
  caml_backtrace_exns_index = 0;
}

void caml_reset_backtrace_exns(void)
{
  int i;
  for (i = 0; i < caml_backtrace_exns_index; ++i)
    Field(caml_backtrace_exns_values, i) = Val_unit;
  caml_backtrace_exns_index = 0;
}

void caml_store_backtrace_exn(value exn)
{
  Assert ((caml_backtrace_exns_values != Val_unit)
          (caml_backtrace_exns_positions != NULL) &&
          (caml_backtrace_exns_index >= 0) &&
          (caml_backtrace_exns_index <= BACKTRACE_EXN_COUNT));

  if (caml_backtrace_pos == 0 ||
      Field(caml_backtrace_exns_values, caml_backtrace_exns_index - 1) != exn) {
    if (caml_backtrace_exns_index < BACKTRACE_EXN_COUNT) {
      Store_field(caml_backtrace_exns_values, caml_backtrace_exns_index, exn);
      caml_backtrace_exns_positions[caml_backtrace_exns_index] = caml_backtrace_pos;
      caml_backtrace_exns_index += 1;
    }
  }
}

/* Start or stop the backtrace machinery */
CAMLprim value caml_record_backtrace(value vflag)
{
  int flag = Int_val(vflag);

  if (flag != caml_backtrace_active) {
    caml_backtrace_active = flag;
    caml_backtrace_pos = 0;

    caml_reset_backtrace_exns();
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
static void print_location(struct caml_loc_info * li, int index, value exn)
{
  /* Messages is before + (if msg ? prefix + msg : "") + after */
  char * before, * prefix, * msg, * after;

  /* Ignore compiler-inserted raise */
  if (!li->loc_valid && li->loc_is_raise) return;

  if (li->loc_is_raise) {
    /* Initial raise if index == 0, re-raise otherwise */
    if (index == 0)
      before = "Raised", prefix = " ", after = " at";
    else
      before = "Re-raised", prefix = " as ", after = " at";
  } else {
    if (index == 0)
      before = "Raised", prefix = " ", after = " by primitive operation at";
    else
      before = "Called", prefix = "", exn = Val_unit, after = " from";
  }

  if (exn != Val_unit)
    msg = caml_format_exception(exn);
  else
    msg = "", prefix = "";

  if (! li->loc_valid) {
    fprintf(stderr, "%s%s%s%s unknown location\n", before, prefix, msg, after);
  } else {
    fprintf (stderr, "%s%s%s%s file \"%s\", line %d, characters %d-%d\n",
             before, prefix, msg, after,
             li->loc_filename, li->loc_lnum,
             li->loc_startchr, li->loc_endchr);
  }

  /* msg has been allocated iff exn != Val_unit */
  if (exn != Val_unit)
    free(msg);
}

/* Print a backtrace */
CAMLexport void caml_print_exception_backtrace(void)
{
  int i, exn_pos;
  struct caml_loc_info li;

  if (!caml_debug_info_available()) {
    fprintf(stderr, "(Cannot print stack backtrace: no debug information available)\n");
    return;
  }

  for (i = 0, exn_pos = 0; i < caml_backtrace_pos; i++) {
    value exn;

    caml_extract_location_info(caml_backtrace_buffer[i], &li);
    if (exn_pos < caml_backtrace_exns_index &&
        caml_backtrace_exns_positions[exn_pos] == i) {
      exn = Field(caml_backtrace_exns_values, exn_pos);
      exn_pos += 1;
    } else
      exn = Val_unit;

    print_location(&li, i, exn);
  }
}

/* Get a copy of the latest backtrace */
CAMLprim value caml_get_exception_raw_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal2(res, pair);

  if (!caml_backtrace_active ||
      caml_backtrace_buffer == NULL ||
      caml_backtrace_pos == 0) {
    res = caml_alloc(0, 0);
  }
  else {
    int i, exn_pos;

    /* Beware: the allocations below may cause finalizers to be run, and another
       backtrace---possibly of a different length---to be stashed (for example if
       the finalizer raises then catches an exception).  We choose to ignore any
       such finalizer backtraces and return the original one. */

    backtrace_slot saved_backtrace_buffer[BACKTRACE_BUFFER_SIZE];
    CAMLlocalN(saved_backtrace_exns_values, BACKTRACE_EXN_COUNT);
    int saved_backtrace_pos, saved_backtrace_exns_index;
    int saved_backtrace_exns_positions[BACKTRACE_EXN_COUNT];

    saved_backtrace_pos = caml_backtrace_pos;
    saved_backtrace_exns_index = caml_backtrace_exns_index;

    if (saved_backtrace_pos > BACKTRACE_BUFFER_SIZE) {
      saved_backtrace_pos = BACKTRACE_BUFFER_SIZE;
    }

    memcpy(saved_backtrace_buffer, caml_backtrace_buffer,
           saved_backtrace_pos * sizeof(backtrace_slot));

    for (i = 0; i < saved_backtrace_exns_index; ++i) {
      saved_backtrace_exns_values[i] = Field(caml_backtrace_exns_values, i);
      saved_backtrace_exns_positions[i] = caml_backtrace_exns_positions[i];
    }

    res = caml_alloc(saved_backtrace_pos, 0);
    for (i = 0, exn_pos = 0; i < saved_backtrace_pos; i++) {
      if ((exn_pos < saved_backtrace_exns_index) &&
          saved_backtrace_exns_positions[exn_pos] == i) {
        pair = caml_alloc(2, 0);
        Field(pair, 0) = caml_val_raw_backtrace_slot(saved_backtrace_buffer[i]);
        Field(pair, 1) = saved_backtrace_exns_values[exn_pos];
        Store_field(res, i, pair);
        exn_pos += 1;
      } else
        Field(res, i) = caml_val_raw_backtrace_slot(saved_backtrace_buffer[i]);
    }
  }

  CAMLreturn(res);
}

/* Convert the raw backtrace to a data structure usable from OCaml */
CAMLprim value caml_convert_raw_backtrace_slot(value backtrace_slot)
{
  CAMLparam1(backtrace_slot);
  CAMLlocal3(p, exn, fname);
  struct caml_loc_info li;

  if (!caml_debug_info_available())
    caml_failwith("No debug information available");

  if (Is_long(backtrace_slot)) {
    exn = Val_unit; /* None */
  } else {
    exn = caml_alloc_small(1, 0); /* Some */
    Field(exn, 0) = Field(backtrace_slot, 1);
    backtrace_slot = Field(backtrace_slot, 0);
  }

  caml_extract_location_info(caml_raw_backtrace_slot_val(backtrace_slot), &li);

  if (li.loc_valid) {
    fname = caml_copy_string(li.loc_filename);
    p = caml_alloc_small(6, 0);
    Field(p, 0) = Val_bool(li.loc_is_raise);
    Field(p, 1) = fname;
    Field(p, 2) = Val_int(li.loc_lnum);
    Field(p, 3) = Val_int(li.loc_startchr);
    Field(p, 4) = Val_int(li.loc_endchr);
    Field(p, 5) = exn;
  } else {
    p = caml_alloc_small(2, 1);
    Field(p, 0) = Val_bool(li.loc_is_raise);
    Field(p, 1) = exn;
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
