module L = Batteries.List
module StringSet = Batteries.Set
module LL = Batteries.LazyList
module E = Batteries.Enum
module H = Batteries.Hashtbl
module SS = Sexplib.Sexp
module SSL = Sexplib.Std
module G = GxlDocument.GxlDocument

open Pretty

exception File_Not_found of string
exception Internal_error
exception Solver_error of string

let (|>) x f = f x in
let weighttbl = H.create 60 in
let usage_msg = "Usage: gxl2smt <filename>\nsee -help for more options" in


let lower_bound = ref 0.0 in
let upper_bound = ref 0.0 in

let build_comm_cond procs s t = 
  LL.init procs (fun x -> LL.init procs (fun y -> if x <> y then "(and Node_"^s^"_"^(string_of_int y) ^ " Node_"^t^"_"^(string_of_int x)^")" else "")) |> LL.concat in

let build_order_cond procs s t =
  LL.init procs (fun x -> LL.init procs (fun y -> if x = y then "(and Node_"^s^"_"^(string_of_int y) ^ " Node_"^t^"_"^(string_of_int x)^")" else "")) |> LL.concat in

(* let output_graph name graph_attrs =  *)
(*   (\* These are the nodes from the allocation *\) *)
(*   (\* let graph_elements =  *\) *)
(*   let graph = GXL.gxl_graph_make ~role:None ~edgeids:None ~hypergraph:None ~edgemode:GXL.Directed ~gxl_type:None ~attrs:graph_attrs  *)
(* 				 ~elements:[] ~id:name in *)
(*   {GXL.graphs=[graph];GXL.xlink="http://www.w3.org/1999/xlink"} in *)

let get_graph_init_and_final_nodes graph_nodes graph_edges =
  let dests = ref [] in
  let sources = ref [] in
  let () = L.iter (fun x -> dests := (GXL.get_edge_target x |> GXL.get_graph_element_id) :: !dests) graph_edges in
  let () = L.iter (fun x -> sources := (GXL.get_edge_source x |> GXL.get_graph_element_id) :: !sources) graph_edges in
  let inits = StringSet.diff (StringSet.of_list graph_nodes)  (StringSet.of_list !dests) |> StringSet.enum |> L.of_enum in
  let finals = StringSet.diff (StringSet.of_list graph_nodes) (StringSet.of_list !sources) |> StringSet.enum |> L.of_enum in
  (inits,finals) in


(* This is the reachability function *)
let reachable gedges a =
  (* Remove from the pot_noreach list if there exists an edge *)
  let marked = ref LL.nil in
  let q = Queue.create () in
  let () = Queue.add a q in
  while not (Queue.is_empty q) do
    let i = Queue.pop q in
    LL.iter (fun x -> 
	       let s = GXL.get_edge_source x |> GXL.get_graph_element_id in
	       let t = GXL.get_edge_target x |> GXL.get_graph_element_id in
	       if s = i then
		 if not (LL.exists (fun y -> t = y) !marked) then 
		   (marked := LL.cons t !marked; 
		    Queue.add t q)
	      ) gedges
  done;
  !marked
in

(* Here gnodes should only be the source nodes in the graph *)
let reachability gnodes gedges = LL.map (function x -> (x,reachable gedges x)) gnodes in

let reach s t closure = 
  let (_,x) = LL.find (function (x,_) -> x = s) closure in
  LL.exists (fun x -> x = t) x in

let add_lower mval ctx = 
  (* First update the Mvalue term with the new argument mval *)
  let m = Z3.mk_app ctx (Z3.mk_func_decl ctx (Z3.mk_string_symbol ctx "M") [||] (Z3.mk_real_sort ctx)) [||] in
  Z3.mk_ge ctx m mval
in

let move_lower mval oval ast ctx = 
  (* First update the Mvalue term with the new argument mval *)
  let m = Z3.mk_app ctx (Z3.mk_func_decl ctx (Z3.mk_string_symbol ctx "M") [||] (Z3.mk_real_sort ctx)) [||] in
  let f = [| Z3.mk_ge ctx m oval |] in
  let t = [| Z3.mk_ge ctx m mval |] in
  Z3.substitute ctx ast f t
in

let add_upper mval ctx = 
  (* First update the Mvalue term with the new argument mval *)
  let m = Z3.mk_app ctx (Z3.mk_func_decl ctx (Z3.mk_string_symbol ctx "M") [||] (Z3.mk_real_sort ctx)) [||] in
  Z3.mk_lt ctx m mval 
in

let move_upper mval oval ast ctx = 
  (* First update the Mvalue term with the new argument mval *)
  let m = Z3.mk_app ctx (Z3.mk_func_decl ctx (Z3.mk_string_symbol ctx "M") [||] (Z3.mk_real_sort ctx)) [||] in
  let f = [| Z3.mk_lt ctx m oval |] in
  let t = [| Z3.mk_lt ctx m mval |] in
  Z3.substitute ctx ast f t
in

let get_values mv = 
  if (Str.string_match (Str.regexp "^(") mv 0) then
    let tt = Str.split (Str.regexp " ") mv in
    (float_of_string (L.nth tt 1)) /. (float_of_string (L.nth (Str.split (Str.regexp ")") (L.nth tt 2)) 0)) 
  else (float_of_string mv)
in

let rec find_optimal debug oval solver ctx ast model = 
  try
    let vv = Z3.solver_check ctx solver in
    (* let () = IFDEF DEBUG THEN print_endline (Z3.ast_vector_to_string ctx (Z3.solver_get_assertions ctx solver)) ELSE () ENDIF in  *)
    match vv with
    | Z3.L_FALSE -> 
       if debug then print_endline (string_of_float (!upper_bound -. !lower_bound));
       if (!upper_bound -. !lower_bound <= 1.0) then
       	 (* let mmodel = Z3.solver_get_model ctx solver in *)
       	 (* let () = if model then (Z3.model_to_string ctx mmodel) |> print_endline else () in *)
       	 print_endline ("Optimal M with reduce: " ^ (string_of_float (!upper_bound) ^ "," ^ (string_of_float 1.0)))
       else
	 let ov = Z3.ast_to_string ctx oval in
	 let ov = get_values ov in
	 let min = !lower_bound in
	 lower_bound := ov;
	 (* calculate the mid again *)
	 let mid = !lower_bound +. ((!upper_bound -. !lower_bound)/.2.0) in
	 (* pop to the original code *)
	 let () = Z3.solver_pop ctx solver ((Z3.solver_get_num_scopes ctx solver)-1) in
	 let () = IFDEF TDEBUG THEN print_endline (Z3.ast_vector_to_string ctx (Z3.solver_get_assertions ctx solver)) ELSE () ENDIF in 
	 let midde = (Z3.mk_numeral ctx (string_of_float !lower_bound) (Z3.mk_real_sort ctx)) in
	 let mmidde = (Z3.mk_numeral ctx (string_of_float min) (Z3.mk_real_sort ctx)) in
	 let () = IFDEF TDEBUG THEN print_endline ((Z3.ast_to_string ctx midde) ^ " Lower bound changed froM NEW to OLD " 
						   ^ (Z3.ast_to_string ctx mmidde)) ELSE () ENDIF in
	 let ast = add_lower midde ctx in
	 let () = Z3.solver_assert ctx solver ast in
	 let () = IFDEF TDEBUG THEN print_endline (Z3.ast_vector_to_string ctx (Z3.solver_get_assertions ctx solver)) ELSE () ENDIF in 
	 let pipi = (Z3.mk_numeral ctx (string_of_float mid) (Z3.mk_real_sort ctx)) in
	 let ast = add_upper pipi ctx in
	 let () = Z3.solver_assert ctx solver ast in
	 let () = IFDEF TDEBUG THEN print_endline (Z3.ast_vector_to_string ctx (Z3.solver_get_assertions ctx solver)) ELSE () ENDIF in 
	 let () = Z3.solver_push ctx solver in
	 find_optimal debug pipi solver ctx ast model

    | Z3.L_TRUE -> 
       let mm = Z3.mk_func_decl ctx (Z3.mk_string_symbol ctx "M") [||] (Z3.mk_real_sort ctx) in
       let mval = (match Z3.model_get_const_interp ctx (Z3.solver_get_model ctx solver) mm with 
		   | None -> raise (Solver_error "Could not find M's value in solver!") 
		   | Some s -> s) in 
       let mv = Z3.ast_to_string ctx mval in
       let ov = Z3.ast_to_string ctx oval in
       let mv = get_values mv in
       let ov = get_values ov in
       let () = if debug then print_endline ((string_of_float mv) ^ " M " ^ (string_of_float ov)) else () in
       let () = if debug then print_endline ((string_of_float !lower_bound) ^ " MLU " ^ (string_of_float !upper_bound) ^ " ") in
       if mv < ov then
	 (* Update ast and call this thing again *)
	 begin
	   if ((int_of_float mv) - (int_of_float !lower_bound) <= 1) then
	     (* found the solution *)
	     let mmodel = Z3.solver_get_model ctx solver in
	     let () = if model then (Z3.model_to_string ctx mmodel) |> print_endline else () in
	     print_endline ("Optimal M with reduce: " ^ (string_of_float (mv) ^ "," ^ (string_of_float 1.0)))
	   else
	     begin
	       upper_bound := !lower_bound +. ((!upper_bound -. !lower_bound)/.2.0);
	       let nv = !lower_bound +. ((!upper_bound -. !lower_bound)/.2.0) in
	       let nv = if nv <= !lower_bound then mv -. 1.0 else nv in
	       let () = if debug then print_endline ("MID" ^ (string_of_float nv)) in
	       let nval = (Z3.mk_numeral ctx (string_of_float nv) (Z3.mk_real_sort ctx)) in
	       let ast = add_upper nval ctx in
	       let () = Z3.solver_push ctx solver in
	       let () = Z3.solver_assert ctx solver ast in
	       find_optimal debug nval solver ctx ast model
	     end
	 end
       else raise Internal_error
    | Z3.L_UNDEF -> 
       print_endline "UNDEF"
       (* raise (Solver_error (Z3.solver_get_reason_unknown ctx solver)) *)
  with
  | Z3.Error(_,Z3.OK) -> print_endline "OK"
  | Z3.Error(_,Z3.EXCEPTION) -> print_endline "EXCEPTION"
  | Z3.Error(_,Z3.IOB) -> print_endline "IOB"
  | Z3.Error(_,Z3.INVALID_ARG) -> print_endline "INVALID_ARG"
  | Z3.Error(_,Z3.PARSER_ERROR) -> print_endline "PARSER_ERROR"
  | Z3.Error(_,Z3.INVALID_PATTERN) -> print_endline "INVALID_PATTERN"
  | Z3.Error(_,Z3.MEMOUT_FAIL) -> print_endline "MEMOUT_FAIL"
  | Z3.Error(_,Z3.FILE_ACCESS_ERROR) -> print_endline "FILE_ACCESS_ERROR"
  | Z3.Error(_,Z3.INTERNAL_FATAL) -> print_endline "INTERNAL_FATAL"
  | Z3.Error(_,Z3.INVALID_USAGE) -> print_endline "M INVALID_USAGE"
  | Z3.Error(_,Z3.DEC_REF_ERROR) -> print_endline "DEC_REF_ERROR"
in

try
  let file_name = ref "" in
  let processors = ref 1 in
  let model = ref false in
  (* let output = ref "" in *)
  let timeout = ref 0 in
  let debug = ref false in
  let speclist = Arg.align [
		     ("-processors", Arg.Set_int processors, " # of processors");
		     ("-debug", Arg.Set debug, " display the reduction in the optimal solution");
		     ("-timeout", Arg.Set_int timeout, " timeout in unsigned integer (ms) [default: 0]");
		     ("-get-alloc", Arg.Set model, " get allocation for the makespan result")
		   ] in
  let () = Arg.parse speclist (fun x -> file_name := x) usage_msg in
  let pp = G.make () in
  let () = if !file_name = "" then raise (File_Not_found !file_name) else () in
  let () = G.parse !file_name pp in
  let gxl_element = G.get_document_element pp in
  let graphs = GXL.get_gxl_gxl_graph_list gxl_element in
  let graph_attrs = L.map GXL.get_typed_element_attr_list graphs in
  let graph_elements = L.map GXL.get_graph_element_list graphs in


  (* These are the graph nodes *)
  let graph_nodes_el = L.map (fun x -> L.filter (function | GXL.GXLNode _ -> true | _ -> false) x) graph_elements in
  let graph_nodes = L.map (fun x -> L.map (fun y -> "Node_"^(GXL.get_graph_element_id y)) x) graph_nodes_el |> L.flatten in
  let declared_node_doc = (L.fold_left append empty) (L.map (fun x -> "(declare-fun " ^ x ^ " () Real)\n") graph_nodes |> L.map text)  in
  let () = IFDEF TDEBUG THEN print declared_node_doc ELSE () ENDIF in

  (* These are the node and the processor combination booleans *)
  let (dnpca_doc,dnpc_doc) = 
    if !processors > 1 then
      let node_processor_combo = L.map (fun x -> L.init !processors (fun i -> x ^ "_" ^ (string_of_int i))) graph_nodes in 
      let dnpc_doc = (L.fold_left append empty) (L.map (fun x -> "(declare-fun " ^ x ^ " () Bool)\n") (L.flatten node_processor_combo) |> L.map text) in
      ((L.fold_left append empty) 
	((L.map (fun x -> "(assert (or " ^ (L.fold_left (fun y z -> z^" "^y) "" x) ^ "))\n") node_processor_combo) |> L.map text),dnpc_doc)
    else (empty,empty) in
  let () = IFDEF TDEBUG THEN print dnpc_doc; print dnpca_doc ELSE () ENDIF in
  
  (* These are the node weights *)
  let node_weights = L.flatten graph_nodes_el |> L.map GXL.get_graph_element_attr_list in
  let node_weights = L.map (fun y -> L.map (fun x -> GXL.get_attr_value x |> GXL.string_of_gxl_value) y) node_weights in
  let ograph_nodes = L.map (fun x -> L.map (fun y -> (GXL.get_graph_element_id y)) x) graph_nodes_el |> L.flatten in
  let () = L.iter2 (fun x y -> H.add weighttbl x y) ograph_nodes node_weights in

  (* These are the graph edges *)
  let graph_edges = L.map (fun x -> L.filter (function | GXL.GXLLocalConnection _ -> true | _ -> false) x) graph_elements in
  let graph_edges = L.map (fun x -> L.map (function | GXL.GXLLocalConnection r -> r | _ -> raise Internal_error) x) graph_edges in
  let ea_doc = 
    (L.fold_left append empty)
      (L.flatten graph_edges |> L.map (fun x -> 
				       (* Assuming that there is only ever 1 thing the weight!! *)
				       let ew = GXL.get_attr_value (GXL.get_local_connection_attr_list x |> L.hd) |> GXL.string_of_gxl_value in
				       let v = GXL.get_edge_source x |> GXL.get_graph_element_id |> H.find weighttbl |> L.hd in 
				       let s = GXL.get_edge_source x |> GXL.get_graph_element_id in
				       let t = GXL.get_edge_target x |> GXL.get_graph_element_id in
				       let orew = 
					 if !processors > 1 then
					   "(ite (or " ^ (LL.fold_left (fun t x -> x ^ " " ^ t) "" (build_comm_cond !processors s t)) ^ ") " ^ ew ^ " 0)"
				       else "0" in
				       (* This needs to be changed for communication *)
				       "(assert (>= Node_" ^ t ^ " (+(+ Node_" ^ s ^ " " ^ v ^ ")"^orew^")))\n" |> text)) in
  let () = IFDEF TDEBUG THEN print ea_doc ELSE () ENDIF in

  (* Make the transtitive clousure *)
  let tclosure = reachability (LL.of_list ograph_nodes) (LL.of_list (L.flatten graph_edges)) in
  (* The ordering constraints *)
  let donee = ref LL.nil in
  let o_doc = 
    L.map (fun s -> 
	   L.map  (fun t -> 
		   if s <> t then
		     if not (reach s t tclosure || reach t s tclosure) then
		       if not (LL.exists (fun x -> x = (s,t) || x = (t,s)) !donee) then
			 begin
			 donee := LL.cons (s,t) !donee;
			 donee := LL.cons (t,s) !donee;
			 let v = H.find weighttbl t |> L.hd in 
			 let sv = H.find weighttbl s |> L.hd in 
			 let orew = 
			   if !processors > 1 then
			     "(ite (or " ^ (LL.fold_left (fun t x -> x ^ " " ^ t) "" (build_order_cond !processors s t)) ^ ") " 
			     ^  "(+ Node_" ^t^" " ^v^")" ^ " 0)"
			   else "(+ Node_" ^t^" " ^v^")" in
			 let rr = 
			   if !processors > 1 then
			     "(ite (or " ^ (LL.fold_left (fun t x -> x ^ " " ^ t) "" (build_order_cond !processors s t)) ^ ") " 
			     ^  "(+ Node_" ^s^" " ^sv^")" ^ " 0)"
			   else "(+ Node_" ^s^" " ^sv^")" in
			 "(assert (or (>= Node_" ^ s ^ orew ^ ")(>= Node_"^t^ " " ^rr^")))\n" |> text 
			 end
		       else empty
		     else empty
		   else empty
		  ) ograph_nodes
	  ) ograph_nodes 
    |> L.flatten |> L.fold_left append empty in
  
  let () = IFDEF TDEBUG THEN print o_doc ELSE () ENDIF in
  (* The final output to the SMT-LIB FORMAT *)
  let top = "(set-logic QF_LRA)\n" |> text in
  let graph_nodes = L.map (fun x -> L.map (fun y -> GXL.get_graph_element_id y) x) graph_nodes_el |> L.flatten in
  let (inits,finals) = get_graph_init_and_final_nodes graph_nodes (L.flatten graph_edges) in
  let oinits = L.map (fun x -> "Node_"^x) inits in 
  let () = IFDEF TDEBUG THEN L.iter (fun x -> print_endline ("init: " ^ x ^ "\n")) inits ELSE () ENDIF in
  let () = IFDEF TDEBUG THEN L.iter (fun x -> print_endline ("final: " ^ x ^ "\n")) finals ELSE () ENDIF in
  let init_inits = L.map (fun x -> "(assert (>= " ^ x ^ " 0))\n" |> text) oinits |> (L.fold_left append empty) in
  let hack1 = "(declare-fun M () Real)\n" |> text in
  let seqt = GXL.string_of_gxl_value
    (match (L.flatten graph_attrs |> L.map (fun x -> if GXL.get_attr_name x = "Total sequential time" then Some (GXL.get_attr_value x) else None) 
		    |> L.filter (function | Some _ -> true | _ -> false) |> L.hd) with | Some x -> x | _ -> raise Internal_error) in
  upper_bound := (float_of_string seqt);
  let hack21 = L.map (fun en -> "(>= M (+ Node_" ^en^" "^(H.find weighttbl en |> L.hd)^")) " |> text) finals |> (L.fold_left append empty) in
  let hack2 = append (append ("(assert (and " |> text) hack21) ("))\n" |> text)  in
  let min = (float_of_string seqt)/. (float_of_int !processors) |> string_of_float in
  lower_bound := (float_of_string min);
  (* let () = print_endline seqt in *)
  let mid = !lower_bound +. ((!upper_bound -. !lower_bound)/.2.0) in
  let mb = "(assert (>= M "^min^"))\n(assert (< M "^(string_of_float mid)^"))\n" |> text in
  let tot = append ea_doc o_doc |> append dnpca_doc |> append mb |> append hack2 |> append init_inits |> append hack1 
	    |> append dnpc_doc |> append declared_node_doc |> append top in
  (* This prints it to screen  *)
  let () = IFDEF DEBUG THEN print tot ELSE () ENDIF in
  let sb = Buffer.create 1000 in
  let out = Buffer.add_string sb  in
  let () = print ~output:out tot in
  let tot = Buffer.contents sb in
  let ctx = Z3.mk_context [("PROOF", "true"); ("MODEL_VALIDATE", "true");("MODEL", "true")] in
  let ast = Z3.parse_smtlib2_string ctx tot [||] [||] [||] [||] in
  let () = IFDEF TDEBUG THEN print_endline (Z3.ast_to_string ctx ast) ELSE () ENDIF in 
  let solver = Z3.mk_solver_for_logic ctx (Z3.mk_string_symbol ctx "QF_LRA") in
  (* Set parameters *)
  let params = Z3.mk_params ctx in
  (* Add the timeout parameter *)
  let () = Z3.params_set_uint ctx params (Z3.mk_string_symbol ctx "soft_timeout") !timeout in
  let () = Z3.params_set_uint ctx params (Z3.mk_string_symbol ctx "ignore_solver1") 1 in
  let () = Z3.params_set_uint ctx params (Z3.mk_string_symbol ctx "timeout") !timeout in
  (* The floyd warshall solver *)
  (* let () = Z3.solver_get_help ctx solver |> print_endline in *)
  let () = Z3.params_set_uint ctx params (Z3.mk_string_symbol ctx "arith.solver") 3 in
  let () = Z3.params_set_uint ctx params (Z3.mk_string_symbol ctx "arith.random_initial_value") 1 in
  let () = Z3.solver_set_params ctx solver params in
  let () = Z3.solver_push ctx solver in
  let () = Z3.solver_assert ctx solver ast in
  let () = IFDEF TDEBUG THEN print_endline (Z3.ast_vector_to_string ctx (Z3.solver_get_assertions ctx solver)) ELSE () ENDIF in 
  let () = find_optimal !debug (Z3.mk_numeral ctx (string_of_float mid) (Z3.mk_real_sort ctx)) solver ctx ast !model in
  Z3.del_context ctx
with
| End_of_file -> exit 0
| Sys_error  _ 
| File_Not_found _ -> Arg.usage [] usage_msg
| _ as s -> raise s
