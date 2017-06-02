(*  TRANSLATION
    + The signature can be translated directly without too much problem
    + The major difficulty with the rules is to translate the dot operator

*)

open Bngl_ast
open Format

let pp_string_option fmt = function
    | None -> ()
    | Some s -> fprintf fmt "%s" s

let prefix_comment c = "  #" ^ c

let pp_annot_list pp_elem fmt = List.iter (function
    | Blank -> fprintf fmt "@;"
    | Comment c -> fprintf fmt "#%s@;" c
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

(* Printing expressions with adequate parens... *)

let binop_priority = function
    | Mult | Div -> 2
    | Add  | Sub -> 1

let unop_priority = function
    | Unary_minus -> 3
    | _ -> 4

let expr_priority = function
    | Var _ | Const _ -> 5
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
    | Div  -> "-"
    | Sub  -> "/"

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



(* Mixtures *)

let site_bonds (name, st) = 
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
    
let pp_int fmt i = fprintf fmt "%d" i

let pp_list_std fmt l = pp_list ~beg_c:"[" ~end_c:"]" ", " fmt l

let pp_graph fmt graph =
    pp_list_std (pp_list_std pp_int) fmt (Array.to_list graph)

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


let pp_site_st nothing_means_free fmt st = 
    st.site_int_state |> List.iter (fun i ->
        fprintf fmt "~%a" pp_identifier i
    ) ;
    match st.site_lnk_state with
    | Any -> fprintf fmt (if nothing_means_free then "!?" else "")
    | Free -> fprintf fmt (if nothing_means_free then "" else "!.")
    | Bound_to_any -> fprintf fmt "!_"
    | Bond i -> fprintf fmt "!%d" i

let pp_site nothing_means_free fmt (s, st) = 
fprintf fmt "%a%a" pp_identifier s (pp_site_st nothing_means_free) st

let pp_agent ?(nothing_means_free=false) fmt ag =
    fprintf fmt "%a(%a)"
        pp_identifier ag.agent_kind
        (pp_list ", " (pp_site nothing_means_free)) ag.agent_sites

let pp_signature_decl fmt ag = 
    fprintf fmt "%%agent: %a" (pp_agent ~nothing_means_free:true) ag

let pp_parameter fmt (id, expr) =
    fprintf fmt "%%var: %a %a" pp_var (fst id) pp_alg_expr expr

let line =
    "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"

let pp_box fmt msg = fprintf fmt "%s@;# %s@;%s@;@;" line msg line

let pp_mixture_cc ?(nothing_means_free=false) fmt cc = 
    pp_list ", " (pp_agent ~nothing_means_free) fmt cc

let pp_mixture fmt m = pp_mixture_cc fmt (List.concat m)


let pp_init_item fmt (cc, expr) =
    fprintf fmt "%%init: %a %a" pp_alg_expr expr (pp_mixture_cc ~nothing_means_free:true) cc


let dumb_naming =
    let counter = ref 0 in
    fun _ -> counter := !counter + 1 ; string_of_int !counter

let pp_rule ?(naming_function=dumb_naming) fmt rule =

    let name = naming_function rule in
    let arrow = if rule.rule_op_rate = None then "->" else "<->" in
    fprintf fmt "'%s' %a %s %a @@ " 
        name pp_mixture rule.rule_lhs arrow pp_mixture rule.rule_rhs ;
    let lhs_stat = mixture_connectivity_status rule.rule_lhs in
    let rhs_stat = mixture_connectivity_status rule.rule_rhs in

    let pp_rate r = function
    | Explicit -> fprintf fmt "%a" pp_alg_expr r
    | Dual_rate -> fprintf fmt "0 { %a }" pp_alg_expr r
    | Non_expressible -> 
        fprintf std_formatter "%a @]@." pp_graph (connection_graph (List.hd rule.rule_lhs)) ;
        let line = (snd rule.rule_name).Locality.from_position.Locality.line in
        failwith (sprintf "Error at line %d: this rule is not expressible in Kappa." line) in

    pp_rate rule.rule_rate lhs_stat ;

    match rule.rule_op_rate with
    | None -> ()
    | Some op_r -> (fprintf fmt ", " ; pp_rate op_r rhs_stat)
    

let pp_obs fmt (kind, id, ccs) =    
    let pp_cc_obs fmt cc = 
        fprintf fmt "|%a|" (pp_mixture_cc ~nothing_means_free:false) cc in
    let pp () = 
        fprintf fmt "%%var: '%a' %a" pp_identifier id (pp_list " + " pp_cc_obs) ccs in
    let warn = " # /!\\ Warning: unable to write monomer observable in Kappa." in

    if List.for_all is_explicitly_connected ccs
    then pp ()
    else begin fprintf fmt "# " ; pp () ; fprintf fmt "%s" warn end


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

let print_kappa fmt model =
    fprintf fmt "@[<v>%a@]@." (pp_annot_list pp_block) model