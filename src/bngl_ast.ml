(*****************************************************************************)
(* FILE                                                                      *)
(*****************************************************************************)

(* We want to preserve some formatting from the original file. *)
type 'a annot_list = ('a item) list
and 'a item =
  (* A blank line *)
  | Blank
  (* An item plus an optional end-of-line comment *)
  | Item of 'a * string option
  (* A full line comment *)
  | Comment of string (* Full line comment *)


type identifier = string Locality.annot

type unop = Unary_minus | Exp

type binop = Mult | Add | Sub | Div

type num_lit = string

type expr =
  | Var of string
  | Unop of unop * expr
  | Binop of expr * binop * expr
  | Const of num_lit
  | App of string * expr list



type lnk_state =
  | Any (* Nothing specified *)
  | Free
  | Bound_to_any
  | Bond of int

type agent = {
  agent_kind : identifier ;
  agent_sites : (identifier * site_descr) list ;
  agent_mod : agent_mod option ; (* for convenience *)
}

and agent_mod = Created | Deleted

and site_descr = {
  site_int_state : identifier list ;
  site_lnk_state : lnk_state ;
  
  site_int_mod : identifier option ;
  site_lnk_mod : lnk_state option ;
  (* Edit notation not available in BNGL: these
     fields are provided for convenience. *)
}

type mixture_cc = agent list

type mixture = mixture_cc list

type function_def =
  { fun_name : identifier ;
    fun_args : identifier list ;
    fun_body : expr ;
  }

type rule =
  { rule_name : identifier option ;
    rule_line : int ;
    rule_lhs : mixture ;
    rule_rhs : mixture ;
    rule_rate : expr ;
    rule_op_rate : expr option ; (* if bidirectional *)

    (* In Kappa, this would always be true.
       In BNGL, if this is set to false, then a deletion won't happen
       if it strictly increases the number of species matched by the rule. *)
    rule_delete_molecules : bool ;
  }

type block =
  | Parameters of parameter annot_list
  | Molecule_types of agent annot_list
  | Seed_species of seed_species_item annot_list
  | Reaction_rules of rule annot_list
  | Observables of observable annot_list
  | Functions of function_def annot_list

and parameter = identifier * expr

and seed_species_item = mixture_cc * expr

(* Kappa observables correspond to `Molecules`.
  With `Species`, each species is counted at most once *)
and observable_type = Molecules | Species

and observable = observable_type * identifier * mixture_cc list

type model = block annot_list