(*  TRANSLATION
    + The signature can be translated directly without too much problem
    + The major difficulty with the rules is to translate the dot operator
    + Functions without arguments are handled correctly for now
    + Use EDIT notation
*)

open Bngl_ast
open Format

let functions_not_handled =
    "Functions with a positive number of arguments are not handled."


(*****************************************************************************)

(* Utilities *)

let pp_string_option fmt = function
    | None -> ()
    | Some s -> fprintf fmt "%s" s

let prefix_comment c = "  //" ^ c

let pp_annot_list pp_elem fmt = List.iter (function
    | Blank -> fprintf fmt "@;"
    | Comment c -> fprintf fmt "//%s@;" c
    | Item (x, c) ->
        fprintf fmt "%a%a@;"
            pp_elem x
            pp_string_option (Utils.map_option prefix_comment c))


let pp_list ?(beg_c="") ?(end_c="") sep pp_elem fmt l =
    let rec aux = function
     | [] -> ()
     | [x] -> fprintf fmt "%a" pp_elem x
     | x::xs -> (fprintf fmt "%a%s" pp_elem x sep; aux xs) in
     fprintf fmt "%s" beg_c ; aux l ; fprintf fmt "%s" end_c

let pp_int fmt i = fprintf fmt "%d" i

let pp_list_std fmt l = pp_list ~beg_c:"[" ~end_c:"]" ", " fmt l

let pp_graph fmt graph =
    pp_list_std (pp_list_std pp_int) fmt (Array.to_list graph)

let line =
    "///////////////////////////////////////////////////////////////////////////////"

let pp_box fmt msg = fprintf fmt "%s@;// %s@;%s@;@;" line msg line


(*****************************************************************************)

(* Printing expressions with adequate parens... *)

let binop_priority = function
    | Mult | Div -> 2
    | Add  | Sub -> 1

let unop_priority = function
    | Unary_minus -> 3
    | _ -> 4

let expr_priority = function
    | Var _ | Const _ | App _ -> 5
    | Unop (op, _) -> unop_priority op
    | Binop (_, op, _) -> binop_priority op

let assoc_binop = function
    | Add | Mult -> true
    | _ -> false

let expr_head_binop = function
    | Binop (_, op, _) -> Some op
    | _ -> None

let expr_not_op = function
    | Binop _ | Unop _ -> false
    | _ -> true

let need_parens binop arg =
    expr_priority arg < binop_priority binop
    || (expr_priority arg = binop_priority binop
    && not (assoc_binop binop && expr_head_binop arg = Some binop))

let pp_var fmt s = fprintf fmt "'%s'" s

let pp_num_lit fmt s = fprintf fmt "%s" s

let pp_identifier fmt s = fprintf fmt "%s" (fst s)

let with_parens pp fmt x = fprintf fmt "(%a)" pp x

let string_of_unop = function
    | Unary_minus -> "-"
    | Exp -> "[exp]"

let string_of_binop = function
    | Mult -> "*"
    | Add  -> "+"
    | Div  -> "/"
    | Sub  -> "-"

let rec pp_alg_expr fmt =
    let pp_with_parens_if b =
        if b then with_parens pp_alg_expr else pp_alg_expr in
    function
    | Var v -> pp_var fmt v
    | Const c -> pp_num_lit fmt c
    | Unop (op, a) ->
        let pp = pp_with_parens_if (op <> Unary_minus) in
        fprintf fmt "%s%a" (string_of_unop op) pp a
    | Binop (lhs, op, rhs) ->
        let ppl = pp_with_parens_if (need_parens op lhs) in
        let ppr = pp_with_parens_if (need_parens op rhs) in
        fprintf fmt "%a %s %a" ppl lhs (string_of_binop op) ppr rhs
    | App (f, args) ->
        if List.length args > 0 then failwith functions_not_handled
        else
            fprintf fmt "%a" pp_var f

(*****************************************************************************)

(* Convert to edit notation *)

let apply_ag_mod modif = 
    List.map (fun ag -> { ag with agent_mod = Some modif } )

let ordered_sites =
    List.sort (fun ((id, _),_) ((id', _),_) -> compare id id')

exception Not_compatible

let agents_diff ag ag' =
    try
        if fst (ag.agent_kind) <> fst (ag'.agent_kind) 
        then raise Not_compatible ;

        let rec aux ss ss' =
            match ss, ss' with
            | [], [] -> []
            | [], _::_ | _::_, [] -> raise Not_compatible 
            | (site, descr)::ss, (site', descr')::ss' ->
                begin
                if fst site <> fst site' then raise Not_compatible ;
                let site_int_mod =
                    match descr.site_int_state, descr'.site_int_state with
                    | [], [] -> None
                    | [(st, loc)], [(st', _)] ->
                        if st <> st' then Some (st', loc) else None
                    | _ -> assert false in
                let site_lnk_mod =
                    if descr.site_lnk_state <> descr'.site_lnk_state then
                        Some descr'.site_lnk_state
                    else None in
                (site, { descr with site_int_mod ; site_lnk_mod }) :: aux ss ss'
                end in

        let agent_sites = 
            aux (ordered_sites ag.agent_sites) (ordered_sites ag'.agent_sites) in
        Some { ag with agent_sites }

    with Not_compatible -> None


let edit_notation m m' = 
    let m  = List.concat m  in
    let m' = List.concat m' in
    let rec still_matching m m' =
        match m, m' with
            | [], _ | _, [] -> not_matching m m'
            | ag::m, ag'::m' ->
                match agents_diff ag ag' with
                    | None -> not_matching (ag::m) (ag'::m') (* Longest common prefix broke *)
                    | Some ag'' -> ag'' :: still_matching m m'
    and not_matching m m' = 
        apply_ag_mod Deleted m @ apply_ag_mod Created m' in
    still_matching m m'


(* Deal with bidirectional rules *)

let op_naming f r = f r ^ "_op"

let expand_and_name_rule naming r =
    match r.rule_op_rate with
    | None -> [(naming r, r)]
    | Some r_op_rate ->
        let r1 = { r with rule_op_rate = None } in
        let r2 = { r with 
            rule_lhs = r.rule_rhs ; 
            rule_rhs = r.rule_lhs ;
            rule_op_rate = None ;
            rule_rate = r_op_rate
            } in
        [(naming r1, r1); (op_naming naming r2, r2)]


(* Count connected components *)

let site_bonds (_name, st) =
    match st.site_lnk_state with
    | Bond i -> [i]
    | _ -> []

let agent_bonds ag =
    ag.agent_sites
    |> List.map site_bonds
    |> List.concat
    |> List.sort_uniq compare

let bfs f i graph =
    let n = Array.length graph in
    let closed = Array.make n false in
    let rec aux = function
        | [] -> ()
        | x::xs ->
            if closed.(x) then aux xs
            else begin
                f x ;
                closed.(x) <- true ;
                aux (graph.(x) @ xs)
            end in
    if i >= 0 && i < n then aux [i]

let graph_number_of_components graph =
    let n = Array.length graph in
    let visited = Array.make n false in
    let n_comps = ref 0 in
    for i = 0 to n-1 do
        if not visited.(i) then begin
            n_comps := !n_comps + 1 ;
            bfs (fun j -> visited.(j) <- true) i graph
        end
    done ;
    !n_comps

let graph_is_connected graph =
    (graph_number_of_components graph <= 1)

let connection_graph cc =
    let per_agent = cc |> List.mapi (fun i ag -> (i, agent_bonds ag)) in
    let graph = Array.make (List.length cc) [] in
    let pending = Hashtbl.create 10 in
    let process_agent (ag, bs) =
        bs |> List.iter (fun b ->
            try begin
                let ag' = Hashtbl.find pending b in
                graph.(ag) <- ag' :: graph.(ag) ;
                graph.(ag') <- ag :: graph.(ag')
            end with Not_found -> Hashtbl.add pending b ag
        ) in
    per_agent |> List.iter process_agent ;
    graph

let is_explicitly_connected cc = graph_is_connected (connection_graph cc)

let number_of_components cc = graph_number_of_components (connection_graph cc)

type mixture_status =
    | Explicit
    | Dual_rate
    | Non_expressible

let mixture_connectivity_status m =
    if List.for_all is_explicitly_connected m then Explicit
    else if List.length m = 1 && number_of_components (List.hd m) = 2
    then Dual_rate
    else Non_expressible


(* Printing rules and mixtures *)

let pp_site_int fmt descr =
    if descr.site_int_state <> [] then
    begin
        fprintf fmt "{%a" (pp_list ", " pp_identifier) descr.site_int_state ;
        begin match descr.site_int_mod with
        | None -> ()
        | Some st -> fprintf fmt "/%a" pp_identifier st end ;
         fprintf fmt "}" 
    end

let pp_site_lnk fmt descr =
    let pp_lnk_state fmt = function
        | Any -> fprintf fmt "#"
        | Free -> fprintf fmt "."
        | Bound_to_any -> fprintf fmt "_"
        | Bond i -> fprintf fmt "%d" i in
    if not (descr.site_lnk_state = Any && descr.site_lnk_mod = None) then
    begin
        fprintf fmt "[" ;
        pp_lnk_state fmt descr.site_lnk_state ;
        begin match descr.site_lnk_mod with
        | None -> ()
    | Some m -> fprintf fmt "/%a" pp_lnk_state m end ;
        fprintf fmt "]"
    end

let pp_site_descr fmt st =
    pp_site_int fmt st ;
    pp_site_lnk fmt st

let pp_site fmt (s, st) =
fprintf fmt "%a%a" pp_identifier s pp_site_descr st

let pp_agent_mod fmt = function
    | None -> ()
    | Some Deleted -> fprintf fmt "-"
    | Some Created -> fprintf fmt "+"

let pp_agent fmt ag =
    fprintf fmt "%a(%a)%a"
        pp_identifier ag.agent_kind
        (pp_list ", " pp_site) ag.agent_sites
        pp_agent_mod ag.agent_mod

let pp_signature_decl fmt ag =
    fprintf fmt "%%agent: %a" pp_agent ag

let pp_parameter fmt (id, expr) =
    fprintf fmt "%%var: %a %a" pp_var (fst id) pp_alg_expr expr

let pp_function_def fmt f =
    let nargs = List.length f.fun_args in
    if nargs > 0 then
        failwith functions_not_handled
    else
        fprintf fmt "%%var: %a %a" pp_var (fst f.fun_name) pp_alg_expr f.fun_body

let pp_mixture_cc fmt cc =
    pp_list ", " pp_agent fmt cc

let pp_mixture fmt m = pp_mixture_cc fmt (List.concat m)


(*****************************************************************************)


let pp_init_item fmt (cc, expr) =
    fprintf fmt "%%init: %a %a" pp_alg_expr expr
        pp_mixture_cc cc

let dumb_naming =
    let counter = ref 0 in
    fun r ->
        match r.rule_name with
        | Some (name, _) -> name
        | None ->
            begin
                counter := !counter + 1 ; string_of_int !counter
            end


let pp_single_rule fmt (name, r) =
    let edit = edit_notation r.rule_lhs r.rule_rhs in
    fprintf fmt "'%s' %a @@ " name pp_mixture_cc edit ;
    match mixture_connectivity_status r.rule_lhs with
    | Explicit -> fprintf fmt "%a" pp_alg_expr r.rule_rate
    | Dual_rate -> fprintf fmt "0 { %a }" pp_alg_expr r.rule_rate
    | Non_expressible ->
        fprintf std_formatter "%a @]@." pp_graph (connection_graph (List.hd r.rule_lhs)) ;
        let line = r.rule_line in
        failwith (sprintf "Error at line %d: this rule is not expressible in Kappa." line)

let pp_rule_gen ~naming_function fmt rule =
    rule
    |> expand_and_name_rule naming_function
    |> pp_list "\n" pp_single_rule fmt

let pp_rule = pp_rule_gen ~naming_function:dumb_naming

let pp_obs fmt (_kind, id, ccs) =
    let pp_cc_obs fmt cc =
        fprintf fmt "|%a|" pp_mixture_cc cc in
    let pp () =
        fprintf fmt "%%var: '%a' %a" pp_identifier id (pp_list " + " pp_cc_obs) ccs in
    let warn = " // /!\\ Warning: unable to write monomer observable in Kappa." in

    if List.for_all is_explicitly_connected ccs
    then pp ()
    else begin fprintf fmt "// " ; pp () ; fprintf fmt "%s" warn end

let pp_block fmt = function
    | Parameters p ->
        pp_box fmt "PARAMETERS" ;
        pp_annot_list pp_parameter fmt p
    | Molecule_types ags ->
        pp_box fmt "SIGNATURE" ;
        pp_annot_list pp_signature_decl fmt ags
    | Seed_species inits ->
        pp_box fmt "INIT" ;
        pp_annot_list pp_init_item fmt inits
    | Reaction_rules rules ->
        pp_box fmt "RULES" ;
        pp_annot_list pp_rule fmt rules
    | Observables obs ->
        pp_box fmt "OBSERVABLES" ;
        pp_annot_list pp_obs fmt obs
    | Functions funs ->
        pp_box fmt "FUNCTIONS" ;
        pp_annot_list pp_function_def fmt funs

let print_kappa fmt model =
    fprintf fmt "@[<v>%a@]@." (pp_annot_list pp_block) model