let fmt fmt' ppf = Format.fprintf ppf fmt'

type 'a t = Format.formatter -> 'a -> unit

let list f ppf l =
  let rec aux ppf = function
    | [x] -> Format.fprintf ppf "@[%a@]" f x
    | [] -> ()
    | x :: xs ->
      Format.fprintf ppf "@[%a@];@ " f x;
      aux ppf xs
  in
  Format.fprintf ppf "@[[@[%a@]]@]" aux l

let placeholder ppf _ =
  Format.pp_print_string ppf "_"

let position ppf {Lexing. pos_fname; pos_lnum; pos_bol; pos_cnum} =
  Format.fprintf ppf
    "@[{ @[pos_fname = %S;@ pos_lnum = %d;@ pos_bol = %d;@ pos_cnum = %d@]@ }@]"
    pos_fname pos_lnum pos_bol pos_cnum

let location ppf {Location. loc_start; loc_end; loc_ghost} =
  Format.fprintf ppf
    "@[{ @[loc_start = %a;@ loc_end = %a;@ loc_ghost = %b@]@ }@]"
    position loc_start position loc_end loc_ghost

let debug_event ppf (ev : Instruct.debug_event) =
  Format.fprintf ppf
    "@[{ @[ev_pos = %d;@ \
     ev_module = %S;@ \
     ev_loc = %a;@ \
     ev_kind = %a;@ \
     ev_info = %a;@ \
     ev_typenv = %a; @ \
     ev_typsubst = %a;@ \
     ev_compenv = %a;@ \
     ev_stacksize = %d;@ \
     ev_repr = %a@]@ \
     }@]"
    ev.ev_pos
    ev.ev_module
    location ev.ev_loc
    placeholder ev.ev_kind
    placeholder ev.ev_info
    placeholder ev.ev_typenv
    placeholder ev.ev_typsubst
    placeholder ev.ev_compenv
    ev.ev_stacksize
    placeholder ev.ev_repr
