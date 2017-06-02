(*****************************************************************************)
(* Lexer                                                                     *)
(*****************************************************************************)


(*  We want to be as faithful as possible to the original file.
    For this, we preserve comments (not indentation though).
    After the model, we do not try to translate. We just add everything as
    a huge comment.

    IDs cannot start with a numeric character.
*)

{

open Format
open Lexing
open Bngl_parser

let keywords_list = 
    [("begin", BEGIN); ("end", END);
     ("model", MODEL); ("parameters", PARAMETERS); ("molecule", MOLECULE);
     ("types", TYPES); ("seed", SEED); ("species", SPECIES); ("Species", SPECIES);
     ("reaction", REACTION); ("rules", RULES); ("observables", OBSERVABLES);
     ("Molecules", MOLECULES); 
     ("exp", EXP);
     ("DeleteMolecules", DELETE_MOLECULES)]

let ktab = 
    let t = Hashtbl.create 20 in 
    keywords_list |> List.iter (fun (k, v) ->
        Hashtbl.add t k v) ;
    t

let kword_or_id s =
    try Hashtbl.find ktab s with Not_found -> ID s

let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let non_null_digit = ['1'-'9']

let integer = "0" | (non_null_digit digit*)


let float_exponent = ['E' 'e'] ['+' '-']? digit+

let float_with_dot = digit* "." (digit*)? float_exponent?

let float_without_dot = digit* float_exponent

let float_number = float_with_dot | float_without_dot


let ident = (letter | '_') (letter | '_' | digit)*

let space = [' ' '\t' '\r']


rule token = parse
  | space {token lexbuf}
  | "\n" {new_line lexbuf; NEW_LINE}
  | "#" ([^'\n']* as comment) {COMMENT comment}

  | "\\" {BACK_SLASH}

  | "(" {OP_PAR}
  | ")" {CL_PAR}
  | "{" {OP_CURL}
  | "}" {CL_CURL}
  | "!" {BANG}
  | "?" {QUESTION_MARK}
  | "~" {TILDE}

  | "->" {ARROW}
  | "<->" {DOUBLE_ARROW}

  | "=" {EQ}
  | ":" {COLON}
  | "," {COMMA}
  | "." {DOT}
  | "_" {UNDERSCORE}

  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {MULT}
  | "/" {DIV}
  
  | float_number as s {FLOAT s}
  | "0" {ZERO}
  | integer as s {POS_INT s}
  | ident as s {kword_or_id s}

  | eof {EOF}
  | _ { failwith (sprintf "Lexer error at line %d" (lexeme_start_p lexbuf).Lexing.pos_lnum) } 


and actions = parse
    | "\n" {new_line lexbuf; NEW_LINE}
    | [^'\n']* as action {ACTION action}

{

    let rec handle_backslashes ?(after_backslash=false) token lexbuf = 
        match token lexbuf with
        | BACK_SLASH -> handle_backslashes ~after_backslash:true token lexbuf
        | NEW_LINE -> if after_backslash then handle_backslashes token lexbuf else NEW_LINE
        | t -> t


    let merge_line_breaks token = 
        let after_newline = ref false in
        let rec token' lexbuf =
            match token lexbuf with
            | NEW_LINE -> 
                if !after_newline then token' lexbuf
                else (after_newline := true ; NEW_LINE)
            | t -> (after_newline := false ; t)
        in token'


    (* Second layer: classify comments. *)

    let classify_comments token = 
        let first_lexem_on_line = ref true in
        let rec token' lexbuf = 
            match token lexbuf with
            | COMMENT c -> 
                if !first_lexem_on_line then FULL_LINE_COMMENT c
                else COMMENT c
            | NEW_LINE -> (first_lexem_on_line := true ; NEW_LINE)
            | t -> (first_lexem_on_line := false ; t)
        in token'

    (* Third layer: unary minus *)

    let handle_unary_minus token =
        let before_binary_minus = function
            | Some CL_PAR -> true
            | Some ZERO -> true
            | Some (POS_INT _) -> true
            | Some (FLOAT _) -> true
            | Some (ID _) -> true
            | _ -> false in
        let last_token = ref None in
        let rec token' lexbuf =
            let new_token =
                match token lexbuf with
                | MINUS ->
                    if before_binary_minus !last_token
                    then MINUS
                    else UNARY_MINUS
                | t -> t in
            last_token := Some new_token ;
            new_token in
        token'


    let lex = token 
        |> handle_backslashes 
        (* |> merge_line_breaks *) 
        |> classify_comments
        |> handle_unary_minus

  }