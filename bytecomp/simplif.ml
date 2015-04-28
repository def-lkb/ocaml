(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Elimination of useless Llet(Alias) bindings.
   Also transform let-bound references into variables. *)

open Asttypes
open Lambda

(* To transform let-bound references into variables *)

exception Real_reference

let rec eliminate_ref id = function
    Lvar v as lam ->
      if Ident.same v id then raise Real_reference else lam
  | Lconst _ as lam -> lam
  | Lapply ap ->
      Lapply{ap with ap_func = eliminate_ref id ap.ap_func;
                     ap_args = List.map (eliminate_ref id) ap.ap_args}
  | Lfunction _ as lam ->
      if IdentSet.mem id (free_variables lam)
      then raise Real_reference
      else lam
  | Llet(str, kind, v, e1, e2) ->
      Llet(str, kind, v, eliminate_ref id e1, eliminate_ref id e2)
  | Lletrec(idel, e2) ->
      Lletrec(List.map (fun (v, e) -> (v, eliminate_ref id e)) idel,
              eliminate_ref id e2)
  | Lprim(Pfield 0, [Lvar v]) when Ident.same v id ->
      Lvar id
  | Lprim(Psetfield(0, _, _), [Lvar v; e]) when Ident.same v id ->
      Lassign(id, eliminate_ref id e)
  | Lprim(Poffsetref delta, [Lvar v]) when Ident.same v id ->
      Lassign(id, Lprim(Poffsetint delta, [Lvar id]))
  | Lprim(p, el) ->
      Lprim(p, List.map (eliminate_ref id) el)
  | Lswitch(e, sw) ->
      Lswitch(eliminate_ref id e,
        {sw_numconsts = sw.sw_numconsts;
         sw_consts =
            List.map (fun (n, e) -> (n, eliminate_ref id e)) sw.sw_consts;
         sw_numblocks = sw.sw_numblocks;
         sw_blocks =
            List.map (fun (n, e) -> (n, eliminate_ref id e)) sw.sw_blocks;
         sw_failaction =
            Misc.may_map (eliminate_ref id) sw.sw_failaction; })
  | Lstringswitch(e, sw, default) ->
      Lstringswitch
        (eliminate_ref id e,
         List.map (fun (s, e) -> (s, eliminate_ref id e)) sw,
         Misc.may_map (eliminate_ref id) default)
  | Lstaticraise (i,args) ->
      Lstaticraise (i,List.map (eliminate_ref id) args)
  | Lstaticcatch(e1, i, e2) ->
      Lstaticcatch(eliminate_ref id e1, i, eliminate_ref id e2)
  | Ltrywith(e1, v, e2) ->
      Ltrywith(eliminate_ref id e1, v, eliminate_ref id e2)
  | Lifthenelse(e1, e2, e3) ->
      Lifthenelse(eliminate_ref id e1,
                  eliminate_ref id e2,
                  eliminate_ref id e3)
  | Lsequence(e1, e2) ->
      Lsequence(eliminate_ref id e1, eliminate_ref id e2)
  | Lwhile(e1, e2) ->
      Lwhile(eliminate_ref id e1, eliminate_ref id e2)
  | Lfor(v, e1, e2, dir, e3) ->
      Lfor(v, eliminate_ref id e1, eliminate_ref id e2,
           dir, eliminate_ref id e3)
  | Lassign(v, e) ->
      Lassign(v, eliminate_ref id e)
  | Lsend(k, m, o, el, loc) ->
      Lsend(k, eliminate_ref id m, eliminate_ref id o,
            List.map (eliminate_ref id) el, loc)
  | Levent(l, ev) ->
      Levent(eliminate_ref id l, ev)
  | Lifused(v, e) ->
      Lifused(v, eliminate_ref id e)

(* Simplification of exits *)

let simplify_exits lam =

  (* Count occurrences of (exit n ...) statements *)
  let exits = Hashtbl.create 17 in

  let count_exit i =
    try
      !(Hashtbl.find exits i)
    with
    | Not_found -> 0

  and incr_exit i =
    try
      incr (Hashtbl.find exits i)
    with
    | Not_found -> Hashtbl.add exits i (ref 1) in

  let rec count = function
  | (Lvar _| Lconst _) -> ()
  | Lapply ap -> count ap.ap_func; List.iter count ap.ap_args
  | Lfunction {body} -> count body
  | Llet(_str, _kind, _v, l1, l2) ->
      count l2; count l1
  | Lletrec(bindings, body) ->
      List.iter (fun (_v, l) -> count l) bindings;
      count body
  | Lprim(_p, ll) -> List.iter count ll
  | Lswitch(l, sw) ->
      count_default sw ;
      count l;
      List.iter (fun (_, l) -> count l) sw.sw_consts;
      List.iter (fun (_, l) -> count l) sw.sw_blocks
  | Lstringswitch(l, sw, d) ->
      count l;
      List.iter (fun (_, l) -> count l) sw;
      begin match  d with
      | None -> ()
      | Some d -> match sw with
        | []|[_] -> count d
        | _ -> count d; count d (* default will get replicated *)
      end
  | Lstaticraise (i,ls) -> incr_exit i ; List.iter count ls
  | Lstaticcatch (l1,(i,[]),Lstaticraise (j,[])) ->
      (* i will be replaced by j in l1, so each occurence of i in l1
         increases j's ref count *)
      count l1 ;
      let ic = count_exit i in
      begin try
        let r = Hashtbl.find exits j in r := !r + ic
      with
      | Not_found ->
          Hashtbl.add exits j (ref ic)
      end
  | Lstaticcatch(l1, (i,_), l2) ->
      count l1;
      (* If l1 does not contain (exit i),
         l2 will be removed, so don't count its exits *)
      if count_exit i > 0 then
        count l2
  | Ltrywith(l1, _v, l2) -> count l1; count l2
  | Lifthenelse(l1, l2, l3) -> count l1; count l2; count l3
  | Lsequence(l1, l2) -> count l1; count l2
  | Lwhile(l1, l2) -> count l1; count l2
  | Lfor(_, l1, l2, _dir, l3) -> count l1; count l2; count l3
  | Lassign(_v, l) -> count l
  | Lsend(_k, m, o, ll, _) -> List.iter count (m::o::ll)
  | Levent(l, _) -> count l
  | Lifused(_v, l) -> count l

  and count_default sw = match sw.sw_failaction with
  | None -> ()
  | Some al ->
      let nconsts = List.length sw.sw_consts
      and nblocks = List.length sw.sw_blocks in
      if
        nconsts < sw.sw_numconsts && nblocks < sw.sw_numblocks
      then begin (* default action will occur twice in native code *)
        count al ; count al
      end else begin (* default action will occur once *)
        assert (nconsts < sw.sw_numconsts || nblocks < sw.sw_numblocks) ;
        count al
      end
  in
  count lam;

  (*
     Second pass simplify  ``catch body with (i ...) handler''
      - if (exit i ...) does not occur in body, suppress catch
      - if (exit i ...) occurs exactly once in body,
        substitute it with handler
      - If handler is a single variable, replace (exit i ..) with it
   Note:
    In ``catch body with (i x1 .. xn) handler''
     Substituted expression is
      let y1 = x1 and ... yn = xn in
      handler[x1 <- y1 ; ... ; xn <- yn]
     For the sake of preserving the uniqueness  of bound variables.
     (No alpha conversion of ``handler'' is presently needed, since
     substitution of several ``(exit i ...)''
     occurs only when ``handler'' is a variable.)
  *)

  let subst = Hashtbl.create 17 in

  let rec simplif = function
  | (Lvar _|Lconst _) as l -> l
  | Lapply ap ->
      Lapply{ap with ap_func = simplif ap.ap_func;
                     ap_args = List.map simplif ap.ap_args}
  | Lfunction{kind; params; body = l; fun_loc; attr} ->
      Lfunction{kind; params; body = simplif l; fun_loc; attr}
  | Llet(str, kind, v, l1, l2) -> Llet(str, kind, v, simplif l1, simplif l2)
  | Lletrec(bindings, body) ->
      Lletrec(List.map (fun (v, l) -> (v, simplif l)) bindings, simplif body)
  | Lprim(p, ll) -> begin
    let ll = List.map simplif ll in
    match p, ll with
        (* Simplify %revapply, for n-ary functions with n > 1 *)
      | Prevapply loc, [x; Lapply ap]
      | Prevapply loc, [x; Levent (Lapply ap,_)] ->
        Lapply {ap with ap_args = ap.ap_args @ [x]; ap_loc = loc}
      | Prevapply loc, [x; f] -> Lapply {ap_should_be_tailcall=false;
                                         ap_loc=loc;
                                         ap_func=f;
                                         ap_args=[x];
                                         ap_inlined=Default_inline;
                                         ap_specialised=Default_specialise}

        (* Simplify %apply, for n-ary functions with n > 1 *)
      | Pdirapply loc, [Lapply ap; x]
      | Pdirapply loc, [Levent (Lapply ap,_); x] ->
        Lapply {ap with ap_args = ap.ap_args @ [x]; ap_loc = loc}
      | Pdirapply loc, [f; x] -> Lapply {ap_should_be_tailcall=false;
                                         ap_loc=loc;
                                         ap_func=f;
                                         ap_args=[x];
                                         ap_inlined=Default_inline;
                                         ap_specialised=Default_specialise}

      | _ -> Lprim(p, ll)
     end
  | Lswitch(l, sw) ->
      let new_l = simplif l
      and new_consts =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_consts
      and new_blocks =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_blocks
      and new_fail = Misc.may_map simplif sw.sw_failaction in
      Lswitch
        (new_l,
         {sw with sw_consts = new_consts ; sw_blocks = new_blocks;
                  sw_failaction = new_fail})
  | Lstringswitch(l,sw,d) ->
      Lstringswitch
        (simplif l,List.map (fun (s,l) -> s,simplif l) sw,
         Misc.may_map simplif d)
  | Lstaticraise (i,[]) as l ->
      begin try
        let _,handler =  Hashtbl.find subst i in
        handler
      with
      | Not_found -> l
      end
  | Lstaticraise (i,ls) ->
      let ls = List.map simplif ls in
      begin try
        let xs,handler =  Hashtbl.find subst i in
        let ys = List.map Ident.rename xs in
        let env =
          List.fold_right2
            (fun x y t -> Ident.add x (Lvar y) t)
            xs ys Ident.empty in
        List.fold_right2
          (fun y l r -> Llet (Alias, Pgenval, y, l, r))
          ys ls (Lambda.subst_lambda env handler)
      with
      | Not_found -> Lstaticraise (i,ls)
      end
  | Lstaticcatch (l1,(i,[]),(Lstaticraise (_j,[]) as l2)) ->
      Hashtbl.add subst i ([],simplif l2) ;
      simplif l1
  | Lstaticcatch (l1,(i,xs),l2) ->
      begin match count_exit i with
      | 0 -> simplif l1
      | 1 when i >= 0 ->
          Hashtbl.add subst i (xs,simplif l2) ;
          simplif l1
      | _ ->
          Lstaticcatch (simplif l1, (i,xs), simplif l2)
      end
  | Ltrywith(l1, v, l2) -> Ltrywith(simplif l1, v, simplif l2)
  | Lifthenelse(l1, l2, l3) -> Lifthenelse(simplif l1, simplif l2, simplif l3)
  | Lsequence(l1, l2) -> Lsequence(simplif l1, simplif l2)
  | Lwhile(l1, l2) -> Lwhile(simplif l1, simplif l2)
  | Lfor(v, l1, l2, dir, l3) ->
      Lfor(v, simplif l1, simplif l2, dir, simplif l3)
  | Lassign(v, l) -> Lassign(v, simplif l)
  | Lsend(k, m, o, ll, loc) ->
      Lsend(k, simplif m, simplif o, List.map simplif ll, loc)
  | Levent(l, ev) -> Levent(simplif l, ev)
  | Lifused(v, l) -> Lifused (v,simplif l)
  in
  simplif lam

(* Compile-time beta-reduction of functions immediately applied:
      Lapply(Lfunction(Curried, params, body), args, loc) ->
        let paramN = argN in ... let param1 = arg1 in body
      Lapply(Lfunction(Tupled, params, body), [Lprim(Pmakeblock(args))], loc) ->
        let paramN = argN in ... let param1 = arg1 in body
   Assumes |args| = |params|.
*)

let beta_reduce params body args =
  List.fold_left2 (fun l param arg -> Llet(Strict, Pgenval, param, arg, l))
                  body params args

(* Simplification of lets *)

let simplify_lets lam =

  (* Disable optimisations for bytecode compilation with -g flag *)
  let optimize = !Clflags.native_code || not !Clflags.debug in

  (* First pass: count the occurrences of all let-bound identifiers *)

  let occ = (Hashtbl.create 83: (Ident.t, int ref) Hashtbl.t) in
  (* The global table [occ] associates to each let-bound identifier
     the number of its uses (as a reference):
     - 0 if never used
     - 1 if used exactly once in and not under a lambda or within a loop
     - > 1 if used several times or under a lambda or within a loop.
     The local table [bv] associates to each locally-let-bound variable
     its reference count, as above.  [bv] is enriched at let bindings
     but emptied when crossing lambdas and loops. *)

  (* Current use count of a variable. *)
  let count_var v =
    try
      !(Hashtbl.find occ v)
    with Not_found ->
      0

  (* Entering a [let].  Returns updated [bv]. *)
  and bind_var bv v =
    let r = ref 0 in
    Hashtbl.add occ v r;
    Tbl.add v r bv

  (* Record a use of a variable *)
  and use_var bv v n =
    try
      let r = Tbl.find v bv in r := !r + n
    with Not_found ->
      (* v is not locally bound, therefore this is a use under a lambda
         or within a loop.  Increase use count by 2 -- enough so
         that single-use optimizations will not apply. *)
    try
      let r = Hashtbl.find occ v in r := !r + 2
    with Not_found ->
      (* Not a let-bound variable, ignore *)
      () in

  let rec count bv = function
  | Lconst _ -> ()
  | Lvar v ->
      use_var bv v 1
  | Lapply{ap_func = Lfunction{kind = Curried; params; body}; ap_args = args}
    when optimize && List.length params = List.length args ->
      count bv (beta_reduce params body args)
  | Lapply{ap_func = Lfunction{kind = Tupled; params; body};
           ap_args = [Lprim(Pmakeblock _, args)]}
    when optimize && List.length params = List.length args ->
      count bv (beta_reduce params body args)
  | Lapply{ap_func = l1; ap_args = ll} ->
      count bv l1; List.iter (count bv) ll
  | Lfunction {body} ->
      count Tbl.empty body
  | Llet(_str, _k, v, Lvar w, l2) when optimize ->
      (* v will be replaced by w in l2, so each occurrence of v in l2
         increases w's refcount *)
      count (bind_var bv v) l2;
      use_var bv w (count_var v)
  | Llet(str, _kind, v, l1, l2) ->
      count (bind_var bv v) l2;
      (* If v is unused, l1 will be removed, so don't count its variables *)
      if str = Strict || count_var v > 0 then count bv l1
  | Lletrec(bindings, body) ->
      List.iter (fun (_v, l) -> count bv l) bindings;
      count bv body
  | Lprim(_p, ll) -> List.iter (count bv) ll
  | Lswitch(l, sw) ->
      count_default bv sw ;
      count bv l;
      List.iter (fun (_, l) -> count bv l) sw.sw_consts;
      List.iter (fun (_, l) -> count bv l) sw.sw_blocks
  | Lstringswitch(l, sw, d) ->
      count bv l ;
      List.iter (fun (_, l) -> count bv l) sw ;
      begin match d with
      | Some d ->
          begin match sw with
          | []|[_] -> count bv d
          | _ -> count bv d ; count bv d
          end
      | None -> ()
      end
  | Lstaticraise (_i,ls) -> List.iter (count bv) ls
  | Lstaticcatch(l1, _, l2) -> count bv l1; count bv l2
  | Ltrywith(l1, _v, l2) -> count bv l1; count bv l2
  | Lifthenelse(l1, l2, l3) -> count bv l1; count bv l2; count bv l3
  | Lsequence(l1, l2) -> count bv l1; count bv l2
  | Lwhile(l1, l2) -> count Tbl.empty l1; count Tbl.empty l2
  | Lfor(_, l1, l2, _dir, l3) -> count bv l1; count bv l2; count Tbl.empty l3
  | Lassign(_v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refcount *)
      count bv l
  | Lsend(_, m, o, ll, _) -> List.iter (count bv) (m::o::ll)
  | Levent(l, _) -> count bv l
  | Lifused(v, l) ->
      if count_var v > 0 then count bv l

  and count_default bv sw = match sw.sw_failaction with
  | None -> ()
  | Some al ->
      let nconsts = List.length sw.sw_consts
      and nblocks = List.length sw.sw_blocks in
      if
        nconsts < sw.sw_numconsts && nblocks < sw.sw_numblocks
      then begin (* default action will occur twice in native code *)
        count bv al ; count bv al
      end else begin (* default action will occur once *)
        assert (nconsts < sw.sw_numconsts || nblocks < sw.sw_numblocks) ;
        count bv al
      end
  in
  count Tbl.empty lam;

  (* Second pass: remove Lalias bindings of unused variables,
     and substitute the bindings of variables used exactly once. *)

  let subst = Hashtbl.create 83 in

(* This (small)  optimisation is always legal, it may uncover some
   tail call later on. *)

  let mklet str kind v e1 e2  = match e2 with
  | Lvar w when optimize && Ident.same v w -> e1
  | _ -> Llet (str, kind,v,e1,e2) in


  let rec simplif = function
    Lvar v as l ->
      begin try
        Hashtbl.find subst v
      with Not_found ->
        l
      end
  | Lconst _ as l -> l
  | Lapply{ap_func = Lfunction{kind = Curried; params; body}; ap_args = args}
    when optimize && List.length params = List.length args ->
      simplif (beta_reduce params body args)
  | Lapply{ap_func = Lfunction{kind = Tupled; params; body};
           ap_args = [Lprim(Pmakeblock _, args)]}
    when optimize && List.length params = List.length args ->
      simplif (beta_reduce params body args)
  | Lapply ap -> Lapply {ap with ap_func = simplif ap.ap_func;
                                 ap_args = List.map simplif ap.ap_args}
  | Lfunction{kind; params; body = l; fun_loc; attr} ->
      begin match simplif l with
        Lfunction{kind=Curried; params=params'; body; attr}
        when kind = Curried && optimize ->
          Lfunction{kind; params = params @ params'; body; fun_loc; attr}
      | body ->
          Lfunction{kind; params; body; fun_loc; attr}
      end
  | Llet(_str, _k, v, Lvar w, l2) when optimize ->
      Hashtbl.add subst v (simplif (Lvar w));
      simplif l2
  | Llet(Strict, kind, v,
         Lprim(Pmakeblock(0, Mutable, kind_ref) as prim, [linit]), lbody)
    when optimize && Config.flambda = false ->
      let slinit = simplif linit in
      let slbody = simplif lbody in
      begin try
        let kind = match kind_ref with
          | None -> Pgenval
          | Some [field_kind] -> field_kind
          | Some _ -> assert false
        in
        mklet Variable kind v slinit (eliminate_ref v slbody)
      with Real_reference ->
        mklet Strict kind v (Lprim(prim, [slinit])) slbody
      end
  | Llet(Alias, kind, v, l1, l2) ->
      begin match count_var v with
        0 -> simplif l2
      | 1 when optimize -> Hashtbl.add subst v (simplif l1); simplif l2
      | _ -> Llet(Alias, kind, v, simplif l1, simplif l2)
      end
  | Llet(StrictOpt, kind, v, l1, l2) ->
      begin match count_var v with
        0 -> simplif l2
      | _ -> mklet Alias kind v (simplif l1) (simplif l2)
      end
  | Llet(str, kind, v, l1, l2) -> mklet str kind v (simplif l1) (simplif l2)
  | Lletrec(bindings, body) ->
      Lletrec(List.map (fun (v, l) -> (v, simplif l)) bindings, simplif body)
  | Lprim(p, ll) -> Lprim(p, List.map simplif ll)
  | Lswitch(l, sw) ->
      let new_l = simplif l
      and new_consts =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_consts
      and new_blocks =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_blocks
      and new_fail = Misc.may_map simplif sw.sw_failaction in
      Lswitch
        (new_l,
         {sw with sw_consts = new_consts ; sw_blocks = new_blocks;
                  sw_failaction = new_fail})
  | Lstringswitch (l,sw,d) ->
      Lstringswitch
        (simplif l,List.map (fun (s,l) -> s,simplif l) sw,
         Misc.may_map simplif d)
  | Lstaticraise (i,ls) ->
      Lstaticraise (i, List.map simplif ls)
  | Lstaticcatch(l1, (i,args), l2) ->
      Lstaticcatch (simplif l1, (i,args), simplif l2)
  | Ltrywith(l1, v, l2) -> Ltrywith(simplif l1, v, simplif l2)
  | Lifthenelse(l1, l2, l3) -> Lifthenelse(simplif l1, simplif l2, simplif l3)
  | Lsequence(Lifused(v, l1), l2) ->
      if count_var v > 0
      then Lsequence(simplif l1, simplif l2)
      else simplif l2
  | Lsequence(l1, l2) -> Lsequence(simplif l1, simplif l2)
  | Lwhile(l1, l2) -> Lwhile(simplif l1, simplif l2)
  | Lfor(v, l1, l2, dir, l3) ->
      Lfor(v, simplif l1, simplif l2, dir, simplif l3)
  | Lassign(v, l) -> Lassign(v, simplif l)
  | Lsend(k, m, o, ll, loc) ->
      Lsend(k, simplif m, simplif o, List.map simplif ll, loc)
  | Levent(l, ev) -> Levent(simplif l, ev)
  | Lifused(v, l) ->
      if count_var v > 0 then simplif l else lambda_unit
  in
  simplif lam

(* Tail call info in annotation files *)

let is_tail_native_heuristic : (int -> bool) ref =
  ref (fun _ -> true)

let rec emit_tail_infos is_tail lambda =
  let call_kind args =
    if is_tail
    && ((not !Clflags.native_code)
        || (!is_tail_native_heuristic (List.length args)))
   then Annot.Tail
   else Annot.Stack in
  match lambda with
  | Lvar _ -> ()
  | Lconst _ -> ()
  | Lapply ap ->
      if ap.ap_should_be_tailcall
      && not is_tail
      && Warnings.is_active Warnings.Expect_tailcall
        then Location.prerr_warning ap.ap_loc Warnings.Expect_tailcall;
      emit_tail_infos false ap.ap_func;
      list_emit_tail_infos false ap.ap_args;
      if !Clflags.annotations then
        Stypes.record (Stypes.An_call (ap.ap_loc, call_kind ap.ap_args))
  | Lfunction {body = lam} ->
      emit_tail_infos true lam
  | Llet (_str, _k, _, lam, body) ->
      emit_tail_infos false lam;
      emit_tail_infos is_tail body
  | Lletrec (bindings, body) ->
      List.iter (fun (_, lam) -> emit_tail_infos false lam) bindings;
      emit_tail_infos is_tail body
  | Lprim (Pidentity, [arg]) ->
      emit_tail_infos is_tail arg
  | Lprim (Psequand, [arg1; arg2])
  | Lprim (Psequor, [arg1; arg2]) ->
      emit_tail_infos false arg1;
      emit_tail_infos is_tail arg2
  | Lprim (_, l) ->
      list_emit_tail_infos false l
  | Lswitch (lam, sw) ->
      emit_tail_infos false lam;
      list_emit_tail_infos_fun snd is_tail sw.sw_consts;
      list_emit_tail_infos_fun snd is_tail sw.sw_blocks;
      Misc.may  (emit_tail_infos is_tail) sw.sw_failaction
  | Lstringswitch (lam, sw, d) ->
      emit_tail_infos false lam;
      List.iter
        (fun (_,lam) ->  emit_tail_infos is_tail lam)
        sw ;
      Misc.may (emit_tail_infos is_tail) d
  | Lstaticraise (_, l) ->
      list_emit_tail_infos false l
  | Lstaticcatch (body, _, handler) ->
      emit_tail_infos is_tail body;
      emit_tail_infos is_tail handler
  | Ltrywith (body, _, handler) ->
      emit_tail_infos false body;
      emit_tail_infos is_tail handler
  | Lifthenelse (cond, ifso, ifno) ->
      emit_tail_infos false cond;
      emit_tail_infos is_tail ifso;
      emit_tail_infos is_tail ifno
  | Lsequence (lam1, lam2) ->
      emit_tail_infos false lam1;
      emit_tail_infos is_tail lam2
  | Lwhile (cond, body) ->
      emit_tail_infos false cond;
      emit_tail_infos false body
  | Lfor (_, low, high, _, body) ->
      emit_tail_infos false low;
      emit_tail_infos false high;
      emit_tail_infos false body
  | Lassign (_, lam) ->
      emit_tail_infos false lam
  | Lsend (_, meth, obj, args, loc) ->
      emit_tail_infos false meth;
      emit_tail_infos false obj;
      list_emit_tail_infos false args;
      if !Clflags.annotations then
        Stypes.record (Stypes.An_call (loc, call_kind (obj :: args)));
  | Levent (lam, _) ->
      emit_tail_infos is_tail lam
  | Lifused (_, lam) ->
      emit_tail_infos is_tail lam
and list_emit_tail_infos_fun f is_tail =
  List.iter (fun x -> emit_tail_infos is_tail (f x))
and list_emit_tail_infos is_tail =
  List.iter (emit_tail_infos is_tail)

(* Split a function with default parameters into a wrapper and an
   inner function.  The wrapper fills in missing optional parameters
   with their default value and tail-calls the inner function.  The
   wrapper can then hopefully be inlined on most call sites to avoid
   the overhead associated with boxing an optional argument with a
   'Some' constructor, only to deconstruct it immediately in the
   function's body. *)

let split_default_wrapper ?(create_wrapper_body = fun lam -> lam)
      fun_id kind params body attr =
  let rec aux map = function
    | Llet(Strict, k, id, (Lifthenelse(Lvar optparam, _, _) as def), rest) when
        Ident.name optparam = "*opt*" && List.mem optparam params
          && not (List.mem_assoc optparam map)
      ->
        let wrapper_body, inner = aux ((optparam, id) :: map) rest in
        Llet(Strict, k, id, def, wrapper_body), inner
    | _ when map = [] -> raise Exit
    | body ->
        (* Check that those *opt* identifiers don't appear in the remaining
           body. This should not appear, but let's be on the safe side. *)
        let fv = Lambda.free_variables body in
        List.iter (fun (id, _) -> if IdentSet.mem id fv then raise Exit) map;

        let inner_id = Ident.create (Ident.name fun_id ^ "_inner") in
        let map_param p = try List.assoc p map with Not_found -> p in
        let args = List.map (fun p -> Lvar (map_param p)) params in
        let wrapper_body =
          Lapply {
            ap_func = Lvar inner_id;
            ap_args = args;
            ap_loc = Location.none;
            ap_should_be_tailcall = false;
            ap_inlined = Default_inline;
            ap_specialised = Default_specialise;
          }
        in
        let inner_params = List.map map_param params in
        let new_ids = List.map Ident.rename inner_params in
        let subst = List.fold_left2
            (fun s id new_id ->
               Ident.add id (Lvar new_id) s)
            Ident.empty inner_params new_ids
        in
        let body = Lambda.subst_lambda subst body in
        let inner_fun =
          Lfunction { kind = Curried; params = new_ids; body; attr;
                      fun_loc = Location.none }
        in
        (wrapper_body, (inner_id, inner_fun))
  in
  let fun_loc = Location.none in
  try
    let wrapper_body, inner = aux [] body in
    let body = create_wrapper_body wrapper_body in
    [(fun_id, Lfunction{kind; params; body; attr; fun_loc}); inner]
  with Exit ->
    [(fun_id, Lfunction{kind; params; body; attr; fun_loc})]

(* trmc rewriting *)

(** RFC
    [map_return f l] apply the mapping function [f] on "return points" of [lam].
    Since lambda is expression-oriented, a lot of things can be considered return points:
    when evaluating (a ; b), both "(a ; b)" and "b" could be considered return
    points, as they both reduces to the final value.
    Therefore the following properties are expected:
    - [f] will not be applied more than once on any branch,
    - all values the lambda can return will go through [f], but [f] will not
      necessarily be applied only on leaves.
    The simplest implementation could be:
      [let map_return f l = f l]

    The actual implementation is finer-grained.
    The main use of [map_return] for trmc is to turn code returning [v] into
    [Psetfield (trmc_offset,v)].
    If there is no benefit in rewriting code such as:
      [if L then va else vb]
    into:
      [if L then Psetfield(..., va) else Psetfield(..., vb)]
    instead of:
      [Psetfield(..., if L then va else vb)]
    then map_return can be dropped.
*)
let rec map_return f =
  let on_assoc (k,v) = k, map_return f v in
  let on_option = function
    | None -> None
    | Some v -> Some (map_return f v)
  in
  function
  | Lvar _ | Lconst _ | Lapply _ | Lfunction _ | Lprim _ | Lsend _
  | Lassign _ | Lfor _ | Lwhile _
    as lam -> f lam
  | Llet (str, k, id, l1, l2) ->
    Llet (str, k, id, l1, map_return f l2)
  | Lletrec (binds, lam) ->
    Lletrec (binds, map_return f lam)
  | Lswitch (l1, sw) ->
    Lswitch (l1,
        {sw with
          sw_consts = List.map on_assoc sw.sw_consts;
          sw_blocks = List.map on_assoc sw.sw_blocks;
          sw_failaction = on_option sw.sw_failaction })
  | Lstringswitch (l1, ls, lo) ->
    Lstringswitch (l1, List.map on_assoc ls, on_option lo)
  | Lstaticraise (id, ls) ->
    Lstaticraise (id, ls)
  | Lstaticcatch (l1, ids, l2) ->
    Lstaticcatch (map_return f l1, ids, map_return f l2)
  | Ltrywith (l1, id, l2) ->
    Ltrywith (map_return f l1, id, map_return f l2)
  | Lifthenelse (l1, l2, l3) ->
    Lifthenelse (l1, map_return f l2, map_return f l3)
  | Lsequence (l1, l2) -> Lsequence (l1, map_return f l2)
  | Levent (lam, lev) -> Levent (map_return f lam, lev)
  | Lifused _ -> assert false

(** [map_exits ~on_return ~on_tail lam] is used to map exit points of the
    lambda, distinguishing between tail positions and non-tail ones.

    Contrary to [map_return], the finer the notion of exit point is the more
    tail calls we can preserve and/or rewrite.
*)
let rec map_exits ~on_return ~on_tail =
  let on lam = map_exits ~on_return ~on_tail lam in
  let on_assoc (k,v) = k, on v in
  let on_option = function
    | None -> None
    | Some v -> Some (on v)
  in
  function
  | Lvar _ | Lconst _ | Lapply _ | Lfunction _ | Lprim _ | Lsend _
  | Lassign _ | Lfor _ | Lwhile _
    as lam -> on_tail lam
  | Llet (str, k, id, l1, l2) ->
    Llet (str, k, id, l1, on l2)
  | Lletrec (binds, lam) ->
    Lletrec (binds, on lam)
  | Lswitch (l1, sw) ->
    Lswitch (l1,
        {sw with
          sw_consts = List.map on_assoc sw.sw_consts;
          sw_blocks = List.map on_assoc sw.sw_blocks;
          sw_failaction = on_option sw.sw_failaction })
  | Lstringswitch (l1, ls, lo) ->
    Lstringswitch (l1, List.map on_assoc ls, on_option lo)
  | Lstaticraise (id, ls) ->
    Lstaticraise (id, ls)
  | Lstaticcatch (l1, ids, l2) ->
    Lstaticcatch (on l1, ids, on l2)
  | Ltrywith (l1, id, l2) ->
    Ltrywith (on_return l1, id, on l2)
  | Lifthenelse (l1, l2, l3) ->
    Lifthenelse (l1, on l2, on l3)
  | Lsequence (l1, l2) -> Lsequence (l1, on l2)
  | Levent (lam, lev) -> Levent (on lam, lev)
  | Lifused _ -> assert false

let trmc_placeholder = Lconst (Const_base (Const_int 0))

let rec find_map f = function
  | [] -> None
  | x :: xs ->
    match f x with
    | Some _ as result -> result
    | None -> find_map f xs

(** All functions on which trmc can be applied in current scope are kept in a
    list, all_candidates, and each is represented as a [trmc_stub].

    Calls to such function with the correct arity are turned into trmc calls.
    After trmc-rewriting, the function takes one more argument which is the
    block to mutate to put the resulting value into.

    The code is parameterized by the offset of the field to be mutated -- the
    body of the function will be duplicated as many times as needed.

    The first pass detect uses of such functions. All different offsets at
    which trmc occurs are kept into the stub_uses field, together with a fresh
    identifier.

    A later pass will duplicate function body, specialize it for mutating the
    offset and bind it to the identifier.
*)

type trmc_stub = {
  stub_arity: int;
  stub_body: lfunction;
  mutable stub_uses: (int * Ident.t) list;
  mutable stub_frozen: bool;
  mutable stub_warned: bool;
}


(* Detection of trmc calls *)

let rec is_reccall all_candidates = function
  | Lapply {ap_func = Lvar id; ap_args; _} ->
      begin match List.assoc id all_candidates with
      | exception Not_found -> None
      | stub ->
        if (stub.stub_body.attr.trmc_candidate || !Clflags.force_trmc) &&
           stub.stub_arity = List.length ap_args
        then Some (id, stub)
        else None
      end
  | Levent (lam,_) -> is_reccall all_candidates lam
  | _ -> None

and is_trmc_call all_candidates = function
  | Lprim (Pmakeblock _, values) ->
      begin match find_map (is_reccall all_candidates) values with
      | Some _ -> true
      | None -> false
      end
  | _ -> false

and has_trmc all_candidates lam =
  is_trmc_call all_candidates lam ||
  (match lam with
   | Levent (lam,_) -> has_trmc all_candidates lam
   | Lprim (Pmakeblock _, values) -> List.exists (has_trmc all_candidates) values
   | _ -> false)

and need_recfunc (id,stub) offset =
  try List.assoc offset stub.stub_uses
  with Not_found ->
    assert (not stub.stub_frozen);
    let id' = Ident.create (Ident.name id ^ "_" ^ string_of_int offset) in
    stub.stub_uses <- (offset, id') :: stub.stub_uses;
    id'

and extract_reccall all_candidates acc = function
  | arg :: args ->
      begin match is_reccall all_candidates arg with
      | None -> extract_reccall all_candidates (arg :: acc) args
      | Some candidate ->
          let offset = List.length acc in
          need_recfunc candidate offset, arg,
          List.rev_append acc (trmc_placeholder :: args)
      end
  | [] -> assert false

and extract_direct_trmc all_candidates = function
  | Lprim (Pmakeblock (tag, _flag, shape), values) ->
      let func, old_app, values' = extract_reccall all_candidates [] values in
      func, old_app, Lprim (Pmakeblock (tag, Mutable, shape), values')
  | _ -> assert false

and extract_trmc all_candidates name lam =
  if is_trmc_call all_candidates lam then
    let result = extract_direct_trmc all_candidates lam in
    result, Lvar name
  else match lam with
    | Levent (lam,lev) ->
        let result, lam = extract_trmc all_candidates name lam in
        result, Levent (lam,lev)
    | Lprim (Pmakeblock (tag, flag, shape), values) ->
        let result, values' = extract_trmc_list all_candidates name [] values in
        result, Lprim (Pmakeblock (tag, flag, shape), values')
    | _ -> assert false

and extract_trmc_list all_candidates name acc = function
  | arg :: args when has_trmc all_candidates arg ->
      let result, arg' = extract_trmc all_candidates name arg in
      result, List.rev_append acc (arg' :: args)
  | arg :: args ->
      extract_trmc_list all_candidates name (arg :: acc) args
  | [] -> assert false

(* Rewriting of trmc calls *)

let on_trmc all_candidates lam =
  if has_trmc all_candidates lam then
    let name_block = Ident.create "trmc_block" in
    let name_result = Ident.create "trmc_result" in
    let (func, old_app, value_block), value_result =
      extract_trmc all_candidates name_block lam
    in
    let rec map_app = function
      | Levent (lam, lev) -> Levent (map_app lam, lev)
      | Lapply ap ->
          let ap_func = Lvar func in
          let ap_args = Lvar name_block :: ap.ap_args in
          Lapply {ap with ap_func; ap_args}
      | _ -> assert false
    in
    let new_app = map_app old_app in
    Llet (Strict, Pgenval, name_block, value_block,
          Llet (Strict, Pgenval, name_result, value_result,
                Lsequence (new_app, Lvar name_result)))
  else lam

(* Traversal and generation of trmc functions *)

let rec introduce_trmc all_candidates bindings =

  let candidates =
    let rec extract = function
      | [] -> []
      | (id, Lfunction lfun) :: rest when lfun.attr.trmc_candidate
                                       || !Clflags.force_trmc ->
          let stub = {
            stub_arity = List.length lfun.params;
            stub_body = lfun;
            stub_uses = [];
            stub_frozen = false;
            stub_warned = false;
          } in
          (id, stub) :: extract rest
      | _ :: rest -> extract rest
    in
    extract bindings
  in

  let all_candidates = candidates @ all_candidates in

  let rewrite_stub_use lfun (offset, id') =
    let caller_block = Ident.create "caller_block" in
    let on_return lam =
      Lprim (Psetfield (offset, Pointer, Initialization),
             [Lvar caller_block; lam])
    in
    let on_tail lam =
      if has_trmc all_candidates lam then
        let name_block = Ident.create "trmc_block" in
        let (func, old_app, value_block), value_result =
          extract_trmc all_candidates name_block lam
        in
        let rec map_app = function
          | Levent (lam, lev) -> Levent (map_app lam, lev)
          | Lapply ap ->
              let ap_func = Lvar func in
              let ap_args = Lvar name_block :: ap.ap_args in
              Lapply {ap with ap_func; ap_args}
          | _ -> assert false
        in
        let new_app = map_app old_app in
        Llet (Strict, Pgenval, name_block, value_block,
              Lsequence (
                Lprim (Psetfield (offset, Pointer, Initialization),
                       [Lvar caller_block; value_result]),
                new_app))
      else on_return lam
    in
    let body = map_exits ~on_return:(map_return on_return) ~on_tail lfun.body in
    id', Lfunction {lfun with params = caller_block :: lfun.params; body}
  in
  let bindings_of_stub (_id, stub) =
    List.map (rewrite_stub_use stub.stub_body) stub.stub_uses
  in

  let rewrite_function lfun =
    let body =
      lfun.body |>
      rewrite_trmc all_candidates |>
      map_exits ~on_return:(fun x -> x) ~on_tail:(on_trmc all_candidates)
    in
    {lfun with body}
  in

  let rewrite_binding = function
    (* Rewrite recursive functions *)
    | id, Lfunction lfun -> id, Lfunction (rewrite_function lfun)
    (* Don't touch other bindings *)
    | binding -> binding
  in

  if candidates = [] then
    List.map rewrite_binding bindings
  else begin
    let bindings = List.map rewrite_binding bindings in
    (* Freeze stub generation, generate all stubs *)
    List.iter (fun (_id, stub) ->
        stub.stub_frozen <- true;
      ) candidates;
    let bindings' = List.concat (List.map bindings_of_stub candidates) in
    bindings' @ bindings
  end

and rewrite_trmc all_candidates =
  let rec aux = function
    | Lletrec (lams, lam) ->
        Lletrec (introduce_trmc all_candidates lams, aux lam)
    | lam -> map_lambda aux lam
  in
  aux

(* The entry point:
   simplification
   + rewriting of trmc-calls
   + emission of tailcall annotations, if needed
*)
let simplify_lambda lam =
  let res = simplify_lets (simplify_exits lam) in
  let res = rewrite_trmc [] res in
  if !Clflags.annotations || Warnings.is_active Warnings.Expect_tailcall
    then emit_tail_infos true res;
  res
