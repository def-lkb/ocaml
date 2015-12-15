(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

module A = Simple_value_approx
module B = Inlining_cost.Benefit
module E = Inline_and_simplify_aux.Env
module R = Inline_and_simplify_aux.Result

let new_var name =
  Variable.create name
    ~current_compilation_unit:(Compilation_unit.get_current_exn ())

let which_function_parameters_can_we_specialise ~params ~args
      ~args_approxs ~(invariant_params:Variable.Set.t Variable.Map.t lazy_t)
      ~specialised_args =
  assert (List.length params = List.length args);
  assert (List.length args = List.length args_approxs);
  List.fold_right2 (fun (var, arg) approx
    (worth_specialising_args, spec_args, args, args_decl) ->
      let spec_args =
        if Variable.Map.mem var (Lazy.force invariant_params) ||
           Variable.Set.mem var specialised_args
        then
          Variable.Map.add var arg spec_args
        else
          spec_args
      in
      let worth_specialising_args =
        if Simple_value_approx.useful approx
          && Variable.Map.mem var (Lazy.force invariant_params)
        then
          Variable.Set.add var worth_specialising_args
        else
          worth_specialising_args
      in
      worth_specialising_args, spec_args, arg :: args, args_decl)
    (List.combine params args) args_approxs
    (Variable.Set.empty, Variable.Map.empty, [], [])

(** Fold over all variables bound by the given closure, which is bound to the
    variable [lhs_of_application], and corresponds to the given
    [function_decls].  Each variable bound by the closure is passed to the
    user-specified function as an [Flambda.named] value that projects the
    variable from its closure. *)
let fold_over_projections_of_vars_bound_by_closure ~closure_id_being_applied
      ~lhs_of_application ~function_decls ~init ~f =
  Variable.Set.fold (fun var acc ->
      let expr : Flambda.named =
        Project_var {
          closure = lhs_of_application;
          closure_id = closure_id_being_applied;
          var = Var_within_closure.wrap var;
        }
      in
      f ~acc ~var ~expr)
    (Flambda_utils.variables_bound_by_the_closure closure_id_being_applied
      function_decls)
    init

let set_inline_attribute_on_all_apply body inline =
  Flambda_iterators.map_toplevel_expr (function
      | Apply apply -> Apply { apply with inline }
      | expr -> expr)
    body

(* CR mshinwell: Add a note somewhere to explain why "bound by the closure"
   does not include the function identifiers for other functions in the same
   set of closures. *)

(** Inline a function by copying its body into a context where it becomes
    closed.  That is to say, we bind the free variables of the body
    (= "variables bound by the closure"), and any function identifiers
    introduced by the corresponding set of closures. *)
let inline_by_copying_function_body ~env ~r ~function_decls ~lhs_of_application
      ~(inline_requested : Lambda.inline_attribute)
      ~closure_id_being_applied
      ~(function_decl : Flambda.function_declaration) ~args ~simplify =
  let r = R.map_benefit r B.remove_call in
  let env =
    (* Don't allow the inlining level to inhibit inlining of stubs (e.g.
       wrappers created by [Unbox_closures]). *)
    if function_decl.stub then env
    else E.inlining_level_up env
  in
  (* Assign fresh names for a function's parameters and update the freshening
     in the environment such that it will rewrite the body to use these new
     names. *)
  let freshened_params, freshening =
    Freshening.add_variables' (E.freshening env) function_decl.params
  in
  let env = E.set_freshening env freshening in
  let body =
    if function_decl.stub && inline_requested <> Lambda.Default_inline then
      (* When the function inlined function is a stub, the annotation
         is reported to the function applications inside the stub.
         This allows to report the annotation to the application the
         original programmer really intended: the stub is not visible
         in the source. *)
      set_inline_attribute_on_all_apply function_decl.body inline_requested
    else
      function_decl.body
  in
  let bindings_for_params_to_args =
    (* Bind the function's parameters to the arguments from the call site. *)
    let args = List.map (fun arg -> Flambda.Expr (Var arg)) args in
    Flambda_utils.bind ~body ~bindings:(List.combine freshened_params args)
  in
  (* Add bindings for the variables bound by the closure. *)
  let bindings_for_vars_bound_by_closure_and_params_to_args =
    fold_over_projections_of_vars_bound_by_closure ~closure_id_being_applied
      ~lhs_of_application ~function_decls ~init:bindings_for_params_to_args
      ~f:(fun ~acc:body ~var ~expr -> Flambda.create_let var expr body)
  in
  (* CR mshinwell: How does this not add a variable that points to the
     function being applied itself?  Presumably it shouldn't do that. *)
  (* Add bindings for variables corresponding to the functions introduced by
     the whole set of closures.  Each such variable will be bound to a closure;
     each such closure is in turn produced by moving from the closure being
     applied to another closure in the same set.
  *)
  let expr =
    Variable.Map.fold (fun another_closure_in_the_same_set _ expr ->
        Flambda.create_let another_closure_in_the_same_set
          (Move_within_set_of_closures {
            closure = lhs_of_application;
            start_from = closure_id_being_applied;
            move_to = Closure_id.wrap another_closure_in_the_same_set;
          })
          expr)
      function_decls.Flambda.funs
      bindings_for_vars_bound_by_closure_and_params_to_args
  in
  let env =
    E.note_entering_closure env ~closure_id:closure_id_being_applied
      ~where:Inline_by_copying_function_body
  in
  simplify (E.activate_freshening env) r expr

let inline_by_copying_function_declaration ~env ~r
    ~(function_decls : Flambda.function_declarations)
    ~lhs_of_application ~closure_id_being_applied
    ~(function_decl : Flambda.function_declaration)
    ~args ~args_approxs ~(invariant_params:Variable.Set.t Variable.Map.t lazy_t)
    ~(specialised_args:Variable.t Variable.Map.t)
    ~dbg ~simplify =
  let specialised_args_set = Variable.Map.keys specialised_args in
  let worth_specialising_args, specialisable_args, args, args_decl =
    which_function_parameters_can_we_specialise
      ~params:function_decl.params ~args ~args_approxs
      ~invariant_params
      ~specialised_args:specialised_args_set
  in
  (* Arguments of functions that are not directly called but are
     aliased to arguments of a directly called one may need to be
     marked as specialiased. *)
  let specialisable_args_with_aliases =
    Variable.Map.fold (fun arg outside_var map ->
        match Variable.Map.find arg (Lazy.force invariant_params) with
        | exception Not_found -> map
        | set ->
            Variable.Set.fold (fun alias map ->
                Variable.Map.add alias outside_var map)
              set map)
      specialisable_args specialisable_args
  in
  (* The other closures from the same set of closures may have
     specialised arguments. Those refer to variables that may not be
     bound anymore in the current environment. The only allowed
     remaining specialised arguments after duplicating a function are
     those that either comes from the free variables of set of
     closures or the arguments of the closure being applied (and
     propagated transitively to other functions). This is ensured by
     the fact that no closure not directly required by the closure
     being applied are kept in the set. If an argument of an other
     function of the set does not come from the closure being applied
     then, that function cannot be applied (unreachable from the one
     being aplied).

     For specialised arguments of other function to reference a valid
     value, they need to be rewritten accordingly to the ones of the
     closure being applied. *)
  let specialisable_renaming =
    Variable.Map.fold (fun param outside_var map ->
        match Variable.Map.find param specialised_args with
        | exception Not_found ->
          (* Newly specialised argument: no other function argument
             may need renaming for that one *)
          map
        | original_outside_var ->
          Variable.Map.add original_outside_var outside_var map)
      specialisable_args_with_aliases Variable.Map.empty
  in
  if Variable.Set.subset worth_specialising_args specialised_args_set
  then
    (* Don't duplicate the function definition if we would make its
       specialisation information worse.  (Note that this judgement is made
       based only on those arguments found to be invariant with known-useful
       approximations, rather than on all invariant arguments.) *)
    None
  else
    let env =
      if function_decl.stub then env
      else E.inlining_level_up env
    in
    let set_of_closures_var = new_var "dup_set_of_closures" in
    (* The free variable map for the duplicated declaration(s) maps the
       "internal" names used within the function bodies to fresh names,
       which in turn are bound to projections from the set of closures being
       copied.  We add these bindings using [Let] around the new
       set-of-closures declaration. *)
    let free_vars, free_vars_for_lets =
      fold_over_projections_of_vars_bound_by_closure ~closure_id_being_applied
        ~lhs_of_application ~function_decls ~init:(Variable.Map.empty, [])
        ~f:(fun ~acc:(map, for_lets) ~var:internal_var ~expr ->
          let from_closure = new_var "from_closure" in
          Variable.Map.add internal_var from_closure map,
            (from_closure, expr)::for_lets)
    in
    let required_functions =
      Flambda_utils.closures_required_by_entry_point ~backend:(E.backend env)
        ~entry_point:closure_id_being_applied
        function_decls
    in
    let funs =
      Variable.Map.filter (fun func _ ->
          Variable.Set.mem func required_functions)
        function_decls.funs
    in
    let function_decls =
      Flambda.update_function_declarations ~funs function_decls
    in
    let all_functions_parameters =
      Flambda_utils.all_functions_parameters function_decls
    in
    let specialisable_args =
      Variable.Map.merge (fun param v1 v2 ->
          match v1, v2 with
          | None, None -> None
          | Some v, _ -> Some v
          | None, Some v ->
            if Variable.Set.mem param all_functions_parameters then
              match Variable.Map.find v specialisable_renaming with
              | exception Not_found ->
                Misc.fatal_errorf
                  "Missing renaming for specialised argument of a function \
                    being duplicated but not directly applied: %a -> %a"
                  Variable.print param Variable.print v
              | argument_from_the_current_application ->
                Some argument_from_the_current_application
            else
              None)
        specialisable_args_with_aliases specialised_args
    in
    let set_of_closures =
      (* This is the new set of closures, with more precise specialisation
         information than the one being copied. *)
      Flambda.create_set_of_closures ~function_decls ~free_vars
        ~specialised_args:specialisable_args
    in
    (* Generate a copy of the function application, including the function
       declaration(s), but with variables (not yet bound) in place of the
       arguments. *)
    let duplicated_application : Flambda.t =
      let project_closure : Flambda.project_closure =
        { set_of_closures = set_of_closures_var;
          closure_id = closure_id_being_applied;
        }
      in
      let func = new_var "dup_func" in
      let body : Flambda.t =
        Flambda.create_let set_of_closures_var
          (Set_of_closures set_of_closures)
          (Flambda.create_let func (Project_closure project_closure)
            (Apply {
              func;
              args;
              kind = Direct closure_id_being_applied;
              dbg;
              inline = function_decl.inline;
            }))
      in
      Flambda_utils.bind ~bindings:free_vars_for_lets ~body
    in
    (* Now bind the variables that will hold the arguments from the original
       application. *)
    let expr : Flambda.t =
      Flambda_utils.bind ~body:duplicated_application ~bindings:args_decl
    in
    let env =
      let closure_ids =
        Closure_id.Set.of_list (
          List.map Closure_id.wrap
            (Variable.Set.elements (Variable.Map.keys function_decls.funs)))
      in
      E.note_entering_closure env ~closure_id:closure_id_being_applied
        ~where:(Inline_by_copying_function_declaration closure_ids)
    in
    Some (simplify (E.activate_freshening env) r expr)