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
#include "caml/backtrace_prim.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "stack.h"

/* In order to prevent the GC from walking through the debug information
   (which have no headers), we transform frame_descr pointers into
   31/63 bits ocaml integers by shifting them by 1 to the right. We do
   not lose information as descr pointers are aligned.  */
value caml_val_raw_backtrace_slot(backtrace_slot pc)
{
  return Val_long((uintnat)pc>>1);
}

backtrace_slot caml_raw_backtrace_slot_val(value v)
{
  return ((backtrace_slot)(Long_val(v)<<1));
}

/* To distinguish between normal backtrace slot (pointing to frame descriptor)
 * and inlined-call slot (pointing to inline entry), the second bit is set
 * (the first one being used for OCaml int tagging).
 * frame descriptors and inline entries are always at least 4 aligned.
 */
static int backtrace_slot_is_inlined(backtrace_slot pc)
{
  return (((uintnat)pc & 2) == 2);
}

static frame_descr * backtrace_slot_get_frame(backtrace_slot pc)
{
  Assert (pc != NULL && !backtrace_slot_is_inlined(pc));
  return pc;
}

static backtrace_slot backtrace_slot_of_frame(frame_descr * d)
{
  Assert (d != NULL && (((uintnat)d & 2) == 0));
  return d;
}

static backtrace_slot backtrace_slot_of_inline_entry(void * d)
{
  Assert (d != NULL && (((uintnat)d & 2) == 0));
  return (backtrace_slot)((uintnat)d | 2);
}

static void * backtrace_slot_get_inline_entry(backtrace_slot pc)
{
  Assert (pc != NULL && backtrace_slot_is_inlined(pc));
  return (void*)((uintnat)pc & ~2);
}

/* Returns the next frame descriptor (or NULL if none is available),
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
      if (d == NULL) return NULL; /* happens if some code compiled without -g */
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

static void *deref_rel(int32_t *ptr, int offset)
{
  ptr += offset;
  if (*ptr == 0)
    return NULL;
  else
    return (char*)ptr + *ptr;
}

static void *extract_infoptr(frame_descr * d)
{
  uintnat infoptr;
  if ((d->frame_size & 1) == 0) {
    return NULL;
  }

  infoptr = ((uintnat) d +
             sizeof(char *) + sizeof(short) + sizeof(short) +
             sizeof(short) * d->num_live + sizeof(frame_descr *) - 1)
            & -sizeof(frame_descr *);

  return (void*)infoptr;
}

static void extract_location_packed_info(uint32_t *infoptr, int is_inlined,
                                  /*out*/ struct caml_loc_info * li)
{
  uint32_t info1, info2;
  info1 = infoptr[0];
  info2 = infoptr[1];

  /* Format of the two info words:
       llllllllllllllllllll aaaaaaaa bbbbbbbbbb nnnnnnnnnnnnnnnnnnnnnnnn i k
                          44       36         26                       2 1 0
                       (32+12)    (32+4)
     k ( 1 bit ): 0 if it's a call, 1 if it's a raise
     i ( 1 bit ): 1 iff call is the entry of an inline chain
     n (24 bits):
      if i == 0, offset (in 4-byte words) of file name relative to infoptr
      else if i == 1, offset (in 4-byte words) of chain entry relative to infoptr
     l (20 bits): line number
     a ( 8 bits): beginning of character range
     b (10 bits): end of character range */
  li->loc_valid = 1;
  li->loc_is_raise = (info1 & 1) != 0;
  li->loc_is_inlined = is_inlined;
  li->loc_lnum = info2 >> 12;
  li->loc_startchr = (info2 >> 4) & 0xFF;
  li->loc_endchr = ((info2 & 0xF) << 6) | (info1 >> 26);

  if ((info1 & 2) == 0) {
    li->loc_filename = (char *)infoptr + (info1 & 0x3FFFFFC);
  } else
  {
    /* Entry to an inline chain cannot be an inlined site itself. */
    Assert (is_inlined == 0);
    void * inline_entry = (char *)infoptr + (info1 & 0x3FFFFFC);
    li->loc_filename = deref_rel(inline_entry, 0);
  }
}

static void *extract_inline_entry(frame_descr * d)
{
  uint32_t * infoptr;
  uint32_t info1;
  void * inline_entry;

  infoptr = extract_infoptr(d);

  if (infoptr == NULL) {
    return NULL;
  }

  info1 = infoptr[0];
  if ((info1 & 2) == 0) {
    return NULL;
  }

  inline_entry = ((char *)infoptr + (info1 & 0x3FFFFFC));
  return deref_rel(inline_entry, 1);
}

static void *extract_inline_next(void * d)
{
  if (d == NULL)
    return NULL;
  else
    return deref_rel(d, 2);
}

CAMLexport void caml_extract_location_info(backtrace_slot slot,
    /*out*/ struct caml_loc_info * li)
{
  uint32_t * infoptr;

  /* Reached end of inline chain */
  if (slot == NULL) {
    li->loc_valid = 0;
    li->loc_is_raise = 1;
    li->loc_is_inlined = 0;
    return;
  }

  if (backtrace_slot_is_inlined(slot)) {
    extract_location_packed_info(backtrace_slot_get_inline_entry(slot), 1, li);
    return;
  }

  infoptr = extract_infoptr(backtrace_slot_get_frame(slot));

  /* If no debugging information available, print nothing.
     When everything is compiled with -g, this corresponds to
     compiler-inserted re-raise operations. */
  if (infoptr == NULL) {
    li->loc_valid = 0;
    li->loc_is_raise = 1;
    li->loc_is_inlined = 0;
    return;
  }

  /* Recover debugging info */
  extract_location_packed_info(infoptr, 0, li);
}

static int count_inline_entries(frame_descr * d)
{
  int i = 0;
  void * iter = extract_inline_entry(d);
  while (iter != NULL) {
    i += 1;
    iter = extract_inline_next(iter);
  }
  return i;
}

int caml_backtrace_count_inlined(backtrace_slot slot)
{
  int counter;
  void * inline_entry;

  if (slot == NULL)
    return 0;

  if (backtrace_slot_is_inlined(slot))
    inline_entry = extract_inline_next(backtrace_slot_get_inline_entry(slot));
  else
    inline_entry = extract_inline_entry(backtrace_slot_get_frame(slot));

  for (counter = 0;
       inline_entry != NULL;
       inline_entry = extract_inline_next(inline_entry))
    counter += 1;
  return counter;
}

backtrace_slot caml_backtrace_next_inlined(backtrace_slot slot)
{
  backtrace_slot next;

  if (slot == NULL)
    return NULL;

  if (backtrace_slot_is_inlined(slot))
    next = extract_inline_next(backtrace_slot_get_inline_entry(slot));
  else
    next = extract_inline_entry(backtrace_slot_get_frame(slot));

  if (next == NULL)
    return NULL;

  return backtrace_slot_of_inline_entry(next);
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
    caml_backtrace_buffer = malloc(BACKTRACE_BUFFER_SIZE * sizeof(backtrace_slot));
    if (caml_backtrace_buffer == NULL) return;
  }

  /* iterate on each frame  */
  while (1) {
    frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
    if (descr == NULL) return;
    /* store its descriptor in the backtrace buffer */
    if (caml_backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;
    caml_backtrace_buffer[caml_backtrace_pos++] = (backtrace_slot) descr;

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
CAMLprim value caml_get_current_callstack(value max_frames_value)
{
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
      trace_size += 1 + count_inline_entries(descr);

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
    void * inlined = NULL;

    for (trace_pos = 0; trace_pos < trace_size; trace_pos++) {
      backtrace_slot slot;
      if (inlined != NULL) {
        slot = backtrace_slot_of_inline_entry(inlined);
        inlined = extract_inline_next(inlined);
      } else {
        frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
        Assert(descr != NULL);
        slot = backtrace_slot_of_frame(descr);
        inlined = extract_inline_entry(descr);
      }
      Store_field(trace, trace_pos, caml_val_raw_backtrace_slot(slot));
    }
  }

  CAMLreturn(trace);
}

int caml_debug_info_available(void)
{
  return 1;
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
      slot = caml_val_raw_backtrace_slot(d);
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
