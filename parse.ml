module L = Batteries.List
module H = Batteries.Hashtbl
module SS = Sexplib.Sexp
module SSL = Sexplib.Std
module G = GxlDocument.GxlDocument

exception File_Not_found of string
exception Internal_error

let (|>) x f = f x;; 



let usage_msg = "Usage: parse <filename>\nsee -help for more options" in
try
  let file_name = ref "" in
  let () = Arg.parse [] (fun x -> file_name := x) usage_msg in
  let pp = G.make () in
  let () = if !file_name = "" then raise (File_Not_found !file_name) else () in
  let () = G.parse !file_name pp in
  let gxl_element = G.get_document_element pp in
  let graphs = GXL.get_gxl_gxl_graph_list gxl_element in
  let () = print_endline "Info about the graph!!" in
  let () = print_endline "Graph names: " in
  let () = L.iter (fun x -> print_endline (GXL.get_typed_element_id x)) graphs in
  let graph_attrs = L.map GXL.get_typed_element_attr_list graphs in
  let () = print_endline "Graph attributes :" in
  let () = L.iter (fun x -> L.iter (fun y -> print_endline ((GXL.get_attr_name y) ^ " = " ^ (GXL.get_attr_value y |> GXL.string_of_gxl_value))) x) graph_attrs in
  let graph_elements = L.map GXL.get_graph_element_list graphs in
  let () = print_endline "Graph node names: " in
  let graph_nodes = L.map (fun x -> L.filter (function | GXL.GXLNode _ as s -> true | _ -> false) x) graph_elements in
  let () = L.iter (fun x -> L.iter (fun y -> print_endline (GXL.get_graph_element_id y)) x) graph_nodes in
  let graph_edges = L.map (fun x -> L.filter (function | GXL.GXLLocalConnection _ as s -> true | _ -> false) x) graph_elements in
  let graph_edges = L.map (fun x -> L.map (function | GXL.GXLLocalConnection r -> r | _ -> raise Internal_error) x) graph_edges in
  let () = print_endline "Connections: " in
  L.iter (fun y -> L.iter (fun z -> (GXL.get_edge_source z |> GXL.get_graph_element_id) ^ " ---> " 
    ^ (GXL.get_edge_target z |> GXL.get_graph_element_id) |> print_endline) y) graph_edges
with
| End_of_file -> exit 0
| Sys_error  _ 
| File_Not_found _ -> Arg.usage [] usage_msg
| _ as s -> raise s
