module L = Batteries.List
module LL = Batteries.LazyList
module E = Batteries.Enum
module H = Batteries.Hashtbl
module SS = Sexplib.Sexp
module SSL = Sexplib.Std
module G = GxlDocument.GxlDocument

open Pretty

exception File_Not_found of string
exception Internal_error

let (|>) x f = f x in
let weighttbl = H.create 60 in
let usage_msg = "Usage: parse <filename>\nsee -help for more options" in

let build_comm_cond procs s t = 
  LL.init procs (fun x -> LL.init procs (fun y -> if x <> y then "(and Node"^s^"P"^(string_of_int y) ^ " Node"^t^"P"^(string_of_int x)^")" else "")) |> LL.concat in

try
  let file_name = ref "" in
  let processors = ref 1 in
  let model = ref false in
  let speclist = Arg.align [
		     ("-processors", Arg.Set_int processors, " # of processors");
		     ("-get-alloc", Arg.Set model, " get allocation for the makespan result")
		   ] in
  let () = Arg.parse speclist (fun x -> file_name := x) usage_msg in
  let pp = G.make () in
  let () = if !file_name = "" then raise (File_Not_found !file_name) else () in
  let () = G.parse !file_name pp in
  let gxl_element = G.get_document_element pp in
  let graphs = GXL.get_gxl_gxl_graph_list gxl_element in
  let () = IFDEF DEBUG THEN print_endline "Info about the graph!!\nGraph names:" ELSE () ENDIF in
  let () = IFDEF DEBUG THEN L.iter (fun x -> print_endline (GXL.get_typed_element_id x)) graphs ELSE () ENDIF in
  let graph_attrs = L.map GXL.get_typed_element_attr_list graphs in
  let () = IFDEF DEBUG THEN print_endline "Graph attributes :" ELSE () ENDIF in
  let () = IFDEF DEBUG THEN 
    L.iter (fun x -> L.iter (fun y -> print_endline ((GXL.get_attr_name y) ^ " = " ^ (GXL.get_attr_value y |> GXL.string_of_gxl_value))) x) graph_attrs 
    ELSE () ENDIF in
  let graph_elements = L.map GXL.get_graph_element_list graphs in


  (* These are the graph nodes *)
  let graph_nodes_el = L.map (fun x -> L.filter (function | GXL.GXLNode _ -> true | _ -> false) x) graph_elements in
  let () = IFDEF DEBUG THEN print_endline "Graph node names: " ELSE () ENDIF in
  let () = IFDEF DEBUG THEN L.iter (fun x -> L.iter (fun y -> print_endline (GXL.get_graph_element_id y)) x) graph_nodes_el ELSE () ENDIF in
  let graph_nodes = L.map (fun x -> L.map (fun y -> "Node"^(GXL.get_graph_element_id y)) x) graph_nodes_el |> L.flatten in
  let declared_node_doc = (L.fold_left append empty) (L.map (fun x -> "(declare-fun " ^ x ^ " () Real)\n") graph_nodes |> L.map text)  in
  let () = IFDEF TDEBUG THEN print declared_node_doc ELSE () ENDIF in

  (* These are the node and the processor combination booleans *)
  let (dnpca_doc,dnpc_doc) = 
    if !processors > 1 then
      let node_processor_combo = L.map (fun x -> L.init !processors (fun i -> x ^ "P" ^ (string_of_int i))) graph_nodes in 
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
  let () = IFDEF DEBUG THEN H.iter (fun x y -> (x ^ "--" ^ (L.fold_left (fun t u -> t ^ " " ^ u) "" y) ) |> print_endline) weighttbl ELSE () ENDIF in

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
				       "(assert (>= Node" ^ t ^ " (+(+ Node" ^ s ^ " " ^ v ^ ")"^orew^")))\n" |> text)) in
  let () = IFDEF TDEBUG THEN print ea_doc ELSE () ENDIF in
  let () = IFDEF DEBUG THEN print_endline "Connections: " ELSE () ENDIF in
  let () = 
    IFDEF DEBUG THEN L.iter (fun y -> L.iter (fun z -> (GXL.get_edge_source z |> GXL.get_graph_element_id) ^ " ---> " 
					  ^ (GXL.get_edge_target z |> GXL.get_graph_element_id) |> print_endline) y) graph_edges ELSE () ENDIF  in
	
  (* The final output to the SMT-LIB FORMAT *)
  let top = "(set-option :produce-proofs true)\n(set-logic QF_LRA)\n" |> text in
  (* The 2 hacks below should be changed -- because they assume that there is a single start node with id 0 and a single sink node with id = No of Nodes - 1 *)
  let hack1 = "(assert (>= Node0 0))\n(declare-fun M () Real)\n" |> text in
  let en = string_of_int (((match (L.flatten graph_attrs |> L.map (fun x -> if GXL.get_attr_name x = "No of nodes" then Some (GXL.get_attr_value x) else None) 
				   |> L.filter (function | Some _ -> true | _ -> false) |> L.hd) with 
			    | Some x -> x | _ -> failwith "fuck!!") |> GXL.string_of_gxl_value |> int_of_string)-1) in
  let seqt = GXL.string_of_gxl_value
    (match (L.flatten graph_attrs |> L.map (fun x -> if GXL.get_attr_name x = "Total sequential time" then Some (GXL.get_attr_value x) else None) 
		    |> L.filter (function | Some _ -> true | _ -> false) |> L.hd) with | Some x -> x | _ -> failwith "fuck!!") in
  let hack2 = "(assert (>= M (+ Node" ^en^" "^(H.find weighttbl en |> L.hd)^")))\n" |> text in
  let mb = "(assert (<= M "^seqt^"))\n" |> text in
  let bot = 
    if !model = false then
      "(check-sat)\n(get-value (M))\n" |> text
  else
      "(check-sat)\n(get-value (M))\n(get-model)\n" |> text in
  let tot = append ea_doc bot |> append dnpca_doc |> append mb |> append hack2 |> append hack1 |> append dnpc_doc |> append declared_node_doc |> append top in
  print tot
  
with
| End_of_file -> exit 0
| Sys_error  _ 
| File_Not_found _ -> Arg.usage [] usage_msg
| _ as s -> raise s
