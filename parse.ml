module L = Batteries.List
module A = Batteries.Array
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
try
  let file_name = ref "" in
  let processors = ref 1 in
  let speclist = Arg.align [("-processors", Arg.Set_int processors, " # of processors")] in
  let () = Arg.parse speclist (fun x -> file_name := x) usage_msg in
  let pp = G.make () in
  let () = if !file_name = "" then raise (File_Not_found !file_name) else () in
  let () = G.parse !file_name pp in
  let gxl_element = G.get_document_element pp in
  let graphs = GXL.get_gxl_gxl_graph_list gxl_element in
  let () = print_endline "Info about the graph!!\nGraph names:" in
  let () = L.iter (fun x -> print_endline (GXL.get_typed_element_id x)) graphs in
  let graph_attrs = L.map GXL.get_typed_element_attr_list graphs in
  let () = print_endline "Graph attributes :" in
  let () = L.iter (fun x -> L.iter (fun y -> print_endline ((GXL.get_attr_name y) ^ " = " ^ (GXL.get_attr_value y |> GXL.string_of_gxl_value))) x) graph_attrs in
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
  let () = IFDEF TDEBUG THEN H.iter (fun x y -> (x ^ "--" ^ (L.fold_left (fun t u -> t ^ " " ^ u) "" y) ) |> print_endline) weighttbl ELSE () ENDIF in

  (* These are the graph edges *)
  let graph_edges = L.map (fun x -> L.filter (function | GXL.GXLLocalConnection _ -> true | _ -> false) x) graph_elements in
  let graph_edges = L.map (fun x -> L.map (function | GXL.GXLLocalConnection r -> r | _ -> raise Internal_error) x) graph_edges in
  let ea_doc = 
    (L.fold_left append empty)
      (L.flatten graph_edges |> L.map (fun x -> 
				       (* Assuming that there is only ever 1 thing the weight!! *)
				       let v = GXL.get_edge_source x |> GXL.get_graph_element_id |> H.find weighttbl |> L.hd in 
				       let s = GXL.get_edge_source x |> GXL.get_graph_element_id in
				       let t = GXL.get_edge_target x |> GXL.get_graph_element_id in
				       (* This needs to be changed for communication *)
				       "(assert (>= Node" ^ t ^ " (+ Node" ^ s ^ " " ^ v ^ ")))\n" |> text)) in
  let () = IFDEF TDEBUG THEN print ea_doc ELSE () ENDIF in
  let () = IFDEF DEBUG THEN print_endline "Connections: " ELSE () ENDIF in
  IFDEF DEBUG THEN
	L.iter (fun y -> L.iter (fun z -> (GXL.get_edge_source z |> GXL.get_graph_element_id) ^ " ---> " 
					  ^ (GXL.get_edge_target z |> GXL.get_graph_element_id) |> print_endline) y) graph_edges ELSE () ENDIF 
with
| End_of_file -> exit 0
| Sys_error  _ 
| File_Not_found _ -> Arg.usage [] usage_msg
| _ as s -> raise s
