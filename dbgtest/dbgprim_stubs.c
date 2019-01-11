#define CAML_INTERNALS

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/fix_code.h>
#include <caml/memory.h>
#include <caml/stacks.h>
#include <caml/backtrace_prim.h>

code_t caml_next_frame_pointer(value ** sp, value ** trsp);

CAMLprim value
dbgprim_with_stack(value f)
{
  CAMLparam1(f);
  CAMLlocal4(stamp,fp,opt,result);

  stamp = Val_unit;

  /* Walk the stack */
  value * sp = caml_extern_sp;
  value * trsp = caml_trapsp;
  code_t p = caml_next_frame_pointer(&sp, &trsp);

  /* Allocate result */
  if (p == NULL)
    opt = Val_unit;
  else
  {
    /* (Some (stamp, code_pointer, stack_pointer, trap_pointer)
     *    : (bool ref * pointer * pointer * pointer) option) */
    stamp = caml_alloc(1, 0);
    Field(stamp, 0) = Val_true;

    fp = caml_alloc(4, 0);
    Field(fp, 0) = stamp;
    Field(fp, 1) = Val_backtrace_slot(p);
    Field(fp, 2) = Val_backtrace_slot(sp);
    Field(fp, 3) = Val_backtrace_slot(trsp);

    opt = caml_alloc(1, 0);
    Field(opt, 0) = fp;
  }

  /* Invoke continuation */
  result = caml_callback_exn(f, opt);

  /* Mark stack handle as invalid */
  if (stamp != Val_unit)
    Field(stamp, 0) = Val_false;

  /* Handle exception continuation */
  if (Is_exception_result(result))
    caml_raise(Extract_exception(result));

  /* Return to ocaml */
  CAMLreturn(result);
}

#define fp_is_valid(fp) (Bool_val(Field(Field(fp,0),0)))

CAMLprim value
dbgprim_is_valid(value fp)
{
  return Field(Field(fp,0),0);
}

CAMLprim value
dbgprim_location(value fp)
{
  code_t p = Backtrace_slot_val(Field(fp, 1));
  return Val_backtrace_slot((p - (code_t)caml_start_code) * sizeof(code_t));
}

CAMLprim value
dbgprim_next(value oldfp)
{
  CAMLparam1(oldfp);
  CAMLlocal2(newfp, opt);

  opt = Val_unit;
  if (fp_is_valid(oldfp))
  {
    value * sp = (value *)Backtrace_slot_val(Field(oldfp, 2));
    value * trsp = (value *)Backtrace_slot_val(Field(oldfp, 3));

    code_t p = caml_next_frame_pointer(&sp, &trsp);

    if (p != NULL)
    {
      newfp = caml_alloc(4, 0);
      Field(newfp, 0) = Field(oldfp, 0);
      Field(newfp, 1) = Val_backtrace_slot(p);
      Field(newfp, 2) = Val_backtrace_slot(sp);
      Field(newfp, 3) = Val_backtrace_slot(trsp);

      opt = caml_alloc(1, 0);
      Field(opt, 0) = newfp;
    }
  }

  CAMLreturn(opt);
}

CAMLprim value
dbgprim_peek(value fp, value pos)
{
  CAMLparam2(fp, pos);
  CAMLlocal2(result, opt);

  opt = Val_unit;
  if (fp_is_valid(fp))
  {
    value * sp = (value *)Backtrace_slot_val(Field(fp, 2));
    result = sp[Long_val(pos)];

    opt = caml_alloc(1, 0);
    Field(opt, 0) = result;
  }

  CAMLreturn(opt);
}

CAMLprim value
dbgprim_peek_stack(value fp, value pos)
{
  CAMLparam2(fp, pos);
  CAMLlocal2(result, opt);

  opt = Val_unit;
  if (fp_is_valid(fp))
  {
    value * sp = (value *)Backtrace_slot_val(Field(fp, 2));
    result = sp[2 + Long_val(pos)];

    opt = caml_alloc(1, 0);
    Field(opt, 0) = result;
  }

  CAMLreturn(opt);
}

CAMLprim value
dbgprim_peek_heap(value fp, value pos)
{
  CAMLparam2(fp, pos);
  CAMLlocal2(result, opt);

  opt = Val_unit;
  if (fp_is_valid(fp))
  {
    value * sp = (value *)Backtrace_slot_val(Field(fp, 2));
    result = ((value**)sp)[0][Long_val(pos)];

    opt = caml_alloc(1, 0);
    Field(opt, 0) = result;
  }

  CAMLreturn(opt);
}

CAMLprim value
dbgprim_peek_rec(value fp, value pos)
{
  CAMLparam2(fp, pos);
  CAMLlocal2(result, opt);

  opt = Val_unit;
  if (fp_is_valid(fp))
  {
    value * sp = (value *)Backtrace_slot_val(Field(fp, 2));
    result = ((value*)sp)[0] + Long_val(pos) * sizeof(value);

    opt = caml_alloc(1, 0);
    Field(opt, 0) = result;
  }

  CAMLreturn(opt);
}

CAMLprim value
dbgprim_log(value log)
{
  (void)log;
  return Val_unit;
}
