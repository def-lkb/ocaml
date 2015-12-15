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

module Constant_defining_value = Flambda.Constant_defining_value

let update_constant_for_sharing sharing_symbol_tbl const : Flambda.constant_defining_value =
  let substitute_symbol sym =
    match Symbol.Tbl.find sharing_symbol_tbl sym with
    | exception Not_found -> sym
    | symbol -> symbol
  in
  match (const:Flambda.constant_defining_value) with
  | Allocated_const _ -> const
  | Block (tag, fields) ->
    let subst_field (field:Flambda.constant_defining_value_block_field) :
      Flambda.constant_defining_value_block_field =
      match field with
      | Const _ -> field
      | Symbol sym ->
        Symbol (substitute_symbol sym)
    in
    let fields = List.map subst_field fields in
    Block (tag, fields)
  | Set_of_closures set_of_closures ->
    Set_of_closures (
      Flambda_iterators.map_symbols_on_set_of_closures
        ~f:substitute_symbol set_of_closures
    )
  | Project_closure (sym, closure_id) ->
    Project_closure (substitute_symbol sym, closure_id)

let cannot_share (const : Flambda.constant_defining_value) =
  match const with
  (* Strings and float arrays are mutable; we never share them. *)
  | Allocated_const ((String _) | (Float_array _)) -> true
  | Allocated_const _ | Set_of_closures _ | Project_closure _ | Block _ ->
    false

let share_definition constant_to_symbol_tbl sharing_symbol_tbl symbol def =
  let def = update_constant_for_sharing sharing_symbol_tbl def in
  if cannot_share def then
    Some def
  else
    begin match Constant_defining_value.Tbl.find constant_to_symbol_tbl def with
    | exception Not_found ->
      Constant_defining_value.Tbl.add constant_to_symbol_tbl def symbol;
      Some def
    | equal_symbol ->
      Symbol.Tbl.add sharing_symbol_tbl symbol equal_symbol;
      None
    end

let share_constants (program : Flambda.program) =
  let sharing_symbol_tbl = Symbol.Tbl.create 42 in
  let constant_to_symbol_tbl = Constant_defining_value.Tbl.create 42 in
  let rec loop (program : Flambda.program_body) : Flambda.program_body =
    match program with
    | Let_symbol (symbol,def,program) ->
      begin match
        share_definition constant_to_symbol_tbl sharing_symbol_tbl symbol def
      with
      | None ->
        loop program
      | Some def' ->
        Let_symbol (symbol,def',loop program)
      end
    | Let_rec_symbol (defs,program) ->
      let defs =
        List.map (fun (symbol, def) ->
            let def = update_constant_for_sharing sharing_symbol_tbl def in
            symbol, def)
          defs
      in
      Let_rec_symbol (defs, loop program)
    | Initialize_symbol (symbol,tag,fields,program) ->
      let fields =
        List.map (fun field ->
            Flambda_iterators.map_symbols
              ~f:(fun symbol ->
                  try Symbol.Tbl.find sharing_symbol_tbl symbol with
                  | Not_found -> symbol)
              field)
          fields
      in
      Initialize_symbol (symbol,tag,fields,loop program)
    | Effect (expr,program) ->
      let expr =
        Flambda_iterators.map_symbols
          ~f:(fun symbol ->
              try Symbol.Tbl.find sharing_symbol_tbl symbol with
              | Not_found -> symbol)
          expr
      in
      Effect (expr, loop program)
    | End root -> End root
  in
  { program with
    program_body = loop program.program_body;
  }