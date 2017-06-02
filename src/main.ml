let in_file = ref ""

let debug_mode = ref false

let usage =
  Sys.argv.(0) ^
  " translates a BNGL model into a Kappa model."

let options = [ 
    "--debug", Arg.Set debug_mode, "set debug mode"
]

let parse_arg arg = 
    if !in_file <> "" then failwith "Only one argument is expected." ;
    in_file := arg

let with_input_file file f =
    if file = "" then f stdin
    else begin
        let ic = open_in file in
        f ic ;
        close_in ic
    end

let main () =
  if !debug_mode then Printexc.record_backtrace true ;
  Arg.parse options parse_arg usage ;
  with_input_file !in_file (fun ic ->
    let lexbuf = Lexing.from_channel ic in
    try
        let ast = Bngl_parser.model Bngl_lexer.lex lexbuf in
        Translate.print_kappa Format.std_formatter ast
        (*print_int (Translate.graph_number_of_components (Array.of_list [[1];[0]]))*)
    with Bngl_parser.Error ->
        failwith (Format.sprintf "Parse error at line %d" (Lexing.lexeme_start_p lexbuf).Lexing.pos_lnum)
  )

let () = main ()