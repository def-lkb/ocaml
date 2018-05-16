#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

#define Tagable Is_block

#ifdef WITH_PROFINFO

CAMLprim value ml_tagl_get_profinfo(value object)
{
  if (Tagable(object))
    return Val_long(Profinfo_val(object));
  else
    return Val_long(0);
}

CAMLprim value ml_tagl_set_profinfo(value object, value tag)
{
  if (Tagable(object))
  {
    header_t hd = Hd_val(object);
    hd = Hd_no_profinfo(hd);
    hd |= ((Long_val(tag) & PROFINFO_MASK) << PROFINFO_SHIFT);
    Hd_val(object) = hd;
    return Val_bool(1);
  }
  else
    return Val_bool(0);
}

#else


CAMLprim value ml_tagl_get_profinfo(value object)
{
  return Val_long(0);
}

CAMLprim value ml_tagl_set_profinfo(value object, value tag)
{
  (void)object;
  (void)tag;
  return Val_bool(0);
}

#endif

CAMLprim value ml_tagl_library(value unit)
{
  (void)unit;
  CAMLparam0();
  CAMLlocal1(ret);

  static value* library;

  if (!library)
    library = caml_named_value("tagl_library");

  if (library)
  {
    ret = caml_alloc(1, 0);
    Store_field(ret, 0, *library);
  }
  else
    ret = Val_long(0);

  CAMLreturn(ret);
}
