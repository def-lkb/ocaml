(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** {0 Representation of types and declarations} *)

(** [Types] defines the representation of types and declarations (that is, the
  content of module signatures).

  Those are all that is needed to express the content of cmi files.

  Notably, typing of term expressions is left to another file, [Typedtree], as
  terms doesn't appear at the signature level.
*)

(** Asttypes exposes basic definitions shared both by Parsetree and Types. *)
open Asttypes

(** Type expressions for the core language.

  The [type_desc] variant defines all the possible type expressions one can
  find in Ocaml. [type_expr] wraps this with some annotations.

  The [level] field tracks the level of polymorphism associated to a type,
  guiding the generalization algorithm.
  Put shortly, when referring to a type in a given environment, both the type
  and the environment have a level. If the type has an higher level, then it
  can be considered fully polymorphic (type variables will be printed as ['a]),
  otherwise it'll be weakly polymorphic, or non generalizable (type variables
  printed as ['_a]).
  See [http://okmij.org/ftp/ML/generalization.html] for more information.

  Note about [type_declaration]: one should not make the confusion between
  [type_expr] and [type_declaration].

  [type_declaration] refers specifically to the [type] construct in ocaml
  language, where you create and name a new type or type alias.

  [type_expr] is used when you refers to existing types, e.g. when annotating
  the expected type of a value.

  Also, as the type system of ocaml is generative, a [type_declaration] can
  have the side-effect of introducing a new type, different from all other
  known types. On the opposite, [type_expr] is a pure construct which allows
  referring to existing types.

  Note on mutability: TBD.
 *)

type type_expr =
  { mutable desc: type_desc;
    mutable level: int;
    mutable id: int }

and type_desc =
  (** [Tvar (Some "a")] ==> ['a] or ['_a]
      [Tvar None]       ==> [_] *)
  | Tvar of string option
  (** [Tarrow (""  , e1, e2, c)] ==> [e1    -> e2]
      [Tarrow ("l",  e1, e2, c)] ==> [l:e1  -> e2]
      [Tarrow ("?l", e1, e2, c)] ==> [?l:e1 -> e2]
    See [commutable] for the last argument. *)
  | Tarrow of label * type_expr * type_expr * commutable
  (** [Ttuple [t1;...;tn]] ==> [(t1 * ... * tn)] *)
  | Ttuple of type_expr list
  (** [Tconstr (`A.B.t', [t1;...;tn], _)] ==> [(t1,...,tn) A.B.t]
      The last parameter keep tracks of known expansions, see [abbrev_memo]. *)
  | Tconstr of Path.t * type_expr list * abbrev_memo ref

  (** [Tobject (`f1:t1;...;fn: tn', `None')] ==> [< f1: t1; ...; fn: tn >]
      f1, fn are represented as a linked list of types using Tfield and Tnil
      constructors.

      [Tobject (_, `Some (`A.ct', [t1;...;tn]')] ==> [(t1, ..., tn) A.ct].
      where A.ct is the type of some class.
  *)
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  (** [Tfield ("foo", Fpresent, t, ts)] ==> [<...; foo : t; ts>] *)
  | Tfield of string * field_kind * type_expr * type_expr
  (** [Tnil] ==> [<...; >] *)
  | Tnil
  (** Indirection used by unification engine. *)
  | Tlink of type_expr
  (** [Tsubst] seems to be used to store information during
      instantiation or copy of a type.
      This constructor should not be used outside of these cases. *)
  | Tsubst of type_expr         (* for copying *)
  (** Representation of polymorphic variant *)
  | Tvariant of row_desc
  | Tunivar of string option
  (** [Tpoly (ty,tyl)] ==> ['a1... 'an. ty],
      where 'a1 ... 'an are names given to types in tyl
      and occurences of those types in ty. *)
  | Tpoly of type_expr * type_expr list
  (* Type of a first-class module (a.k.a package). *)
  | Tpackage of Path.t * Longident.t list * type_expr list

and row_desc =
    { row_fields: (label * row_field) list;
      row_more: type_expr;
      row_bound: unit; (* kept for compatibility *)
      row_closed: bool;
      row_fixed: bool;
      row_name: (Path.t * type_expr list) option }

and row_field =
    Rpresent of type_expr option
  | Reither of bool * type_expr list * bool * row_field option ref
        (* 1st true denotes a constant constructor *)
        (* 2nd true denotes a tag in a pattern matching, and
           is erased later *)
  | Rabsent

(** [abbrev_memo] allows one to keep track of different expansions of a type
    alias.

    For instance, when defining [type 'a pair = 'a * 'a], when one refers to an
    ['a pair], it is just a shortcut for the ['a * 'a] type.
    This expansion will be stored in the [abbrev_memo] of the corresponding
    [Tconstr] node.

    In practice, [abbrev_memo] behaves like list of expansions with a mutable
    tail.

    Note on marshalling: [abbrev_memo] must not appear in saved types.
    [Btype], with [cleanup_abbrev] and [memo], takes care of tracking and
    removing abbreviations.
*)
and abbrev_memo =
  (** No known abbrevation *)
  | Mnil
  (** Found one abbreviation.
    A valid abbreviation should be at least as visible and
    reachable by the same path.
    The first expression is the abbreviation and the second the expansion. *)
  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
  (** Abbreviations can be found after this indirection *)
  | Mlink of abbrev_memo ref

and field_kind =
    Fvar of field_kind option ref
  | Fpresent
  | Fabsent

(** [commutable] is a flag appended to every arrow type.

    It's purpose is to carry information about what is known about the order of
    applied arguments.

    When typing an application (e.g [f x]), it is needed to infer the type of
    the function being applied. However in presence of labels there is no
    longer a strict order imposed on arguments.

    As such, when applying a labelled argument we keep track of the fact that
    the exact order is not known. When unifying with the correct actual
    function type, the order is recovered.

    When typing an abstraction, the correct order is always known
    (e.g [fun ~a ~b -> ...] always produce [Cok] arguments).
    Conversely, generalising a type containing [Cunknown] arguments is not a
    good sign, and means that the order of arguments gets arbitrarily fixed.
    There is no principal typing in presence of unordered, potentially optional
    arguments. (FIXME is this the correct interpretation?)
*)
and commutable =
    Cok
  | Cunknown
  | Clink of commutable ref

module TypeOps : sig
  type t = type_expr
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

(* Maps of methods and instance variables *)

module Meths : Map.S with type key = string
module Vars  : Map.S with type key = string

(* Value descriptions *)

type value_description =
  { val_type: type_expr;                (* Type of the value *)
    val_kind: value_kind;
    val_loc: Location.t;
    val_attributes: Parsetree.attributes;
   }

and value_kind =
    Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)
  | Val_ivar of mutable_flag * string   (* Instance variable (mutable ?) *)
  | Val_self of (Ident.t * type_expr) Meths.t ref *
                (Ident.t * mutable_flag * virtual_flag * type_expr) Vars.t ref *
                string * type_expr
                                        (* Self *)
  | Val_anc of (string * Ident.t) list * string
                                        (* Ancestor *)
  | Val_unbound                         (* Unbound variable *)

(* Constructor descriptions *)

type constructor_description =
  { cstr_name: string;                  (* Constructor name *)
    cstr_res: type_expr;                (* Type of the result *)
    cstr_existentials: type_expr list;  (* list of existentials *)
    cstr_args: type_expr list;          (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: constructor_tag;          (* Tag for heap blocks *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int;                (* Number of non-const constructors *)
    cstr_normal: int;                   (* Number of non generalized constrs *)
    cstr_generalized: bool;             (* Constrained return type? *)
    cstr_private: private_flag;         (* Read-only constructor? *)
    cstr_loc: Location.t;
    cstr_attributes: Parsetree.attributes;
   }

and constructor_tag =
    Cstr_constant of int                (* Constant constructor (an int) *)
  | Cstr_block of int                   (* Regular constructor (a block) *)
  | Cstr_exception of Path.t * Location.t (* Exception constructor *)

(* Record label descriptions *)

type label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutable_flag;              (* Is this a mutable field? *)
    lbl_pos: int;                       (* Position in block *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation;  (* Representation for this record *)
    lbl_private: private_flag;          (* Read-only field? *)
    lbl_loc: Location.t;
    lbl_attributes: Parsetree.attributes;
  }

and record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)

(* Variance *)

module Variance : sig
  type t
  type f = May_pos | May_neg | May_weak | Inj | Pos | Neg | Inv
  val null : t                          (* no occurence *)
  val full : t                          (* strictly invariant *)
  val covariant : t                     (* strictly covariant *)
  val may_inv : t                       (* maybe invariant *)
  val union  : t -> t -> t
  val inter  : t -> t -> t
  val subset : t -> t -> bool
  val set : f -> bool -> t -> t
  val mem : f -> t -> bool
  val conjugate : t -> t                (* exchange positive and negative *)
  val get_upper : t -> bool * bool                  (* may_pos, may_neg   *)
  val get_lower : t -> bool * bool * bool * bool    (* pos, neg, inv, inj *)
end

(* Type definitions *)

type type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_kind;
    type_private: private_flag;
    type_manifest: type_expr option;
    type_variance: Variance.t list;
    (* covariant, contravariant, weakly contravariant, injective *)
    type_newtype_level: (int * int) option;
    (* definition level * expansion level *)
    type_loc: Location.t;
    type_attributes: Parsetree.attributes;
  }

and type_kind =
    Type_abstract
  | Type_record of label_declaration list  * record_representation
  | Type_variant of constructor_declaration list

and label_declaration =
  {
    ld_id: Ident.t;
    ld_mutable: mutable_flag;
    ld_type: type_expr;
    ld_loc: Location.t;
    ld_attributes: Parsetree.attributes;
  }

and constructor_declaration =
  {
    cd_id: Ident.t;
    cd_args: type_expr list;
    cd_res: type_expr option;
    cd_loc: Location.t;
    cd_attributes: Parsetree.attributes;
  }

and type_transparence =
    Type_public      (* unrestricted expansion *)
  | Type_new         (* "new" type *)
  | Type_private     (* private type *)

type exception_declaration =
    { exn_args: type_expr list;
      exn_loc: Location.t;
      exn_attributes: Parsetree.attributes;
    }

(* Type expressions for the class language *)

module Concr : Set.S with type elt = string

type class_type =
    Cty_constr of Path.t * type_expr list * class_type
  | Cty_signature of class_signature
  | Cty_arrow of label * type_expr * class_type

and class_signature =
  { csig_self: type_expr;
    csig_vars:
      (Asttypes.mutable_flag * Asttypes.virtual_flag * type_expr) Vars.t;
    csig_concr: Concr.t;
    csig_inher: (Path.t * type_expr list) list }

type class_declaration =
  { cty_params: type_expr list;
    mutable cty_type: class_type;
    cty_path: Path.t;
    cty_new: type_expr option;
    cty_variance: Variance.t list;
    cty_loc: Location.t;
    cty_attributes: Parsetree.attributes;
  }

type class_type_declaration =
  { clty_params: type_expr list;
    clty_type: class_type;
    clty_path: Path.t;
    clty_variance: Variance.t list;
    clty_loc: Location.t;
    clty_attributes: Parsetree.attributes;
  }

(* Type expressions for the module language *)

type module_type =
    Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of Ident.t * module_type option * module_type
  | Mty_alias of Path.t

and signature = signature_item list

and signature_item =
    Sig_value of Ident.t * value_description
  | Sig_type of Ident.t * type_declaration * rec_status
  | Sig_exception of Ident.t * exception_declaration
  | Sig_module of Ident.t * module_declaration * rec_status
  | Sig_modtype of Ident.t * modtype_declaration
  | Sig_class of Ident.t * class_declaration * rec_status
  | Sig_class_type of Ident.t * class_type_declaration * rec_status

and module_declaration =
  {
    md_type: module_type;
    md_attributes: Parsetree.attributes;
    md_loc: Location.t;
  }

and modtype_declaration =
  {
    mtd_type: module_type option;  (* None: abstract *)
    mtd_attributes: Parsetree.attributes;
    mtd_loc: Location.t;
  }

and rec_status =
    Trec_not                            (* not recursive *)
  | Trec_first                          (* first in a recursive group *)
  | Trec_next                           (* not first in a recursive group *)
