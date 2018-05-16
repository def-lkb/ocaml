#include <caml/mlvalues.h>

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
