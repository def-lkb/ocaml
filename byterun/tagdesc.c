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
#include "caml/stacks.h"
#include "caml/sys.h"
#include "caml/backtrace.h"
#include "caml/fail.h"
#include "caml/backtrace_prim.h"

#ifndef NATIVE_CODE

CAMLprim value caml_read_tag_section(value unit)
{
  (void)unit;
  CAMLparam0();
  CAMLlocal1(library);
  char_os *exec_name;
  int fd, num_events, orig, i;
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
  (void)unit;
  return Val_unit;
}

#endif
