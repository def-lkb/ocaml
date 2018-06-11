#define CAML_INTERNALS
#include "caml/config.h"

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/io.h"
#include "caml/instruct.h"
#include "caml/intext.h"
#include "caml/exec.h"
#include "caml/fix_code.h"
#include "caml/memory.h"
#include "caml/startup.h"
#include "caml/stack.h"
#include "caml/stacks.h"
#include "caml/sys.h"
#include "caml/backtrace.h"
#include "caml/fail.h"
#include "caml/backtrace_prim.h"

#include <limits.h>

#ifndef NATIVE_CODE

CAMLprim value caml_read_tag_section(value unit)
{
  (void)unit;
  CAMLparam0();
  CAMLlocal1(library);
  char_os *exec_name;
  int fd;
  struct channel *chan;
  struct exec_trailer trail;

  if (caml_cds_file != NULL) {
    exec_name = caml_cds_file;
  } else {
    exec_name = caml_exe_name;
  }

  fd = caml_attempt_open(&exec_name, &trail, 1);
  if (fd < 0)
    CAMLreturn(Val_unit);

  caml_read_section_descriptors(fd, &trail);
  if (caml_seek_optional_section(fd, &trail, "TAGL") != -1) {
    chan = caml_open_descriptor_in(fd);
    library = caml_input_val(chan);
    caml_close_channel(chan);
  }

  CAMLreturn(library);
}

#else

CAMLprim value caml_read_tag_section(value unit)
{
  return caml_input_value_from_block(caml_globals_taglib, INT_MAX);
}

#endif

CAMLprim value caml_compiler_tags(value unit)
{
  (void)unit;
  static value tag_ref = 0;

  if (tag_ref == 0)
  {
    tag_ref = caml_alloc(1, 0);
    Store_field(tag_ref, 0, Val_unit);
    caml_register_generational_global_root(&tag_ref);
  }

  return tag_ref;
}
