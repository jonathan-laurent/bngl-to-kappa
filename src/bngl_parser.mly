(*****************************************************************************)
(* Parser                                                                    *)
(*****************************************************************************)

%{

open Bngl_ast

let flatten_opt_annot_list = List.map (function
  | Item (None, None) -> Blank
  | Item (None, Some c) -> Comment c
  | Item (Some x, c) -> Item (x, c)
  | Comment c -> Comment c
  | Blank -> Blank )


let nothing_means_any ag =
  let process st =
    match st with
    | Free -> Any
    | Any | Bond _ | Bound_to_any -> st in
  let agent_sites = 
    List.map (fun (s, descr) ->
        (s, { descr with site_lnk_state = process descr.site_lnk_state })
    ) ag.agent_sites in
  { ag with agent_sites }

let dummy_rule = {
    rule_name = None ;
    rule_line = 0 ;
    rule_lhs = [] ;
    rule_rhs = [] ;
    rule_rate = Const "0" ;
    rule_op_rate = None ;
    rule_delete_molecules = false ;
  }

  let add_pos x =
    (x, Locality.of_pos
      (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()))

  let cur_line () =
    (Parsing.symbol_start_pos ()).Lexing.pos_lnum

%}


%token NEW_LINE
%token BACK_SLASH (* Eliminated by the lexer *)
%token BEGIN END
%token MODEL PARAMETERS MOLECULE TYPES SEED SPECIES
%token REACTION RULES OBSERVABLES MOLECULES FUNCTIONS

%token ZERO
%token <string> POS_INT
%token <string> FLOAT
%token <string> ID
%token <string> LABEL

%token <string> COMMENT
%token <string> FULL_LINE_COMMENT

%token OP_PAR CL_PAR
%token BANG QUESTION_MARK TILDE
%token EQ COMMA DOT

%token UNARY_MINUS
%token PLUS MINUS MULT DIV
%token EXP

%token ARROW DOUBLE_ARROW
%token DELETE_MOLECULES

(*%left COMMA*)
%left PLUS MINUS
%left MULT DIV
%nonassoc UNARY_MINUS
(*%nonassoc EXP*)

%start model
%type <Bngl_ast.model> model

%%


/* Util */

%inline int:
  | ZERO { "0" }
  | i=POS_INT { i }

%inline annot_list(elem): its=list(annot_list_item(elem)) { its }

%inline annot_list_item(elem):
  | NEW_LINE { Blank }
  | c=FULL_LINE_COMMENT NEW_LINE { Comment c }
  | e=elem c=option(COMMENT) NEW_LINE { Item (e, c) }

%inline line_and_break(elem): e=elem option(COMMENT) NEW_LINE { e }

%inline id_or_int:
  | s=ID { s }
  | ZERO { "0" }
  | s=POS_INT { s }

%inline identifier: id=ID { add_pos id }

/* Expressions */

num:
  | s=int { s }
  | s=FLOAT { s }

%inline binop:
  | MULT { Mult }
  | PLUS { Add }
  | MINUS { Sub }
  | DIV { Div }

%inline unop:
  | UNARY_MINUS { Unary_minus }

%inline unary_function:
  | EXP { Exp }

alg_expr:
  | n=num { Const n }
  | v=ID { Var v }
  | lhs=alg_expr op=binop rhs=alg_expr { Binop (lhs, op, rhs) }
  | op=unop arg=alg_expr { Unop (op, arg) }
  | OP_PAR e=alg_expr CL_PAR { e }
  | f=unary_function OP_PAR arg=alg_expr CL_PAR { Unop (f, arg) }
  | id=ID OP_PAR args=separated_list(COMMA, alg_expr) CL_PAR { App (id, args) }




/* Agents and mixtures */

mixture:
  | ZERO { [] }
  | ccs=separated_nonempty_list(PLUS, mixture_cc) { ccs }

mixture_cc: ags=separated_nonempty_list(DOT, agent) { ags }

agent:
  | agent_kind=ID OP_PAR agent_sites=separated_list(COMMA, site) CL_PAR
    { { agent_kind=(add_pos agent_kind) ; agent_sites ; agent_mod=None } }

site: id=ID st=site_state { (add_pos id, st) }

site_state:
  | { {site_int_state=[]; site_lnk_state=Free; site_int_mod=None; site_lnk_mod=None} }
  | site_int_state=nonempty_list(site_int_state_annot) site_lnk_state=site_lnk_annot
    { {site_int_state; site_lnk_state; site_int_mod=None; site_lnk_mod=None} }
  | site_lnk_state=preceded(BANG, site_lnk) site_int_state=list(site_int_state_annot)
    { {site_int_state; site_lnk_state; site_int_mod=None; site_lnk_mod=None} }

%inline site_int_state_annot: TILDE st=id_or_int { add_pos st }

%inline site_lnk_annot:
  | { Free }
  | BANG sl=site_lnk { sl }

%inline site_lnk:
  | s=int { Bond (int_of_string s) }
  | MULT { Any }
  | PLUS { Bound_to_any }
  | QUESTION_MARK { Any }



/* Model blocks */

parameters:
  | line_and_break(BEGIN PARAMETERS {})
    params=annot_list(parameter)
    END PARAMETERS
    { Parameters params }

parameter: name=ID option(EQ) value=alg_expr { (add_pos name, value) }

molecule_types:
  | line_and_break(BEGIN MOLECULE TYPES {})
    mts=annot_list(ag=agent {nothing_means_any ag})
    END MOLECULE TYPES
    { Molecule_types mts }

seed_species:
  | line_and_break(BEGIN SEED SPECIES {})
    ss=annot_list(one_seed_species)
    END SEED SPECIES
    { Seed_species ss }

one_seed_species: sp=mixture_cc qt=alg_expr { (List.map nothing_means_any sp, qt) }

reaction_rules:
  | line_and_break(BEGIN REACTION RULES {})
    rrs=annot_list(rule)
    END REACTION RULES
    { Reaction_rules rrs }


rule_name:
  | n=POS_INT { add_pos n }
  | id=LABEL { add_pos id }

(* Rules can be assigned a positive integer as a label in
  old versions of BNGL. *)
rule_start: rule_name=option(rule_name) rule_lhs=mixture
  { fun r -> {r with rule_name ; rule_lhs ; rule_line = cur_line ()} }

rule_end:
  | ARROW rule_rhs=mixture rule_rate=alg_expr
    rule_delete_molecules=boption(DELETE_MOLECULES)
    { fun r -> {r with rule_rhs ; rule_rate ; rule_delete_molecules} }
  | DOUBLE_ARROW rule_rhs=mixture rule_rate=alg_expr COMMA rule_op_rate=alg_expr
    rule_delete_molecules=boption(DELETE_MOLECULES)
    { fun r -> {r with rule_rhs ; rule_rate ; rule_op_rate=Some(rule_op_rate) ; rule_delete_molecules} }

rule: rs=rule_start re=rule_end { re (rs dummy_rule) }

%inline obs_kind:
  | SPECIES { Species }
  | MOLECULES { Molecules }

observables:
  | line_and_break(BEGIN OBSERVABLES {})
    obs=annot_list(observable)
    END OBSERVABLES
    { Observables obs }

observable: kind=obs_kind id=ID mixts=separated_list(option(COMMA),mixture_cc)
  { (kind, add_pos id, List.map (List.map nothing_means_any) mixts) }

functions:
  | line_and_break(BEGIN FUNCTIONS {})
    fs=annot_list(function_def)
    END FUNCTIONS
    { Functions fs }

function_def:
  fun_name=identifier OP_PAR
  fun_args=separated_list(COMMA, identifier) CL_PAR EQ
  fun_body=alg_expr
  { {fun_name ; fun_args ; fun_body} }


/* Model */

model: model=annot_list(model_block) END MODEL { flatten_opt_annot_list model }

model_block:
  | BEGIN MODEL { None }
  | b=parameters { Some b }
  | b=molecule_types { Some b }
  | b=seed_species { Some b }
  | b=reaction_rules { Some b }
  | b=observables { Some b }
  | b=functions { Some b }