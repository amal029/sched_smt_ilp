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

let (|>) x f = f x in
let usage_msg = "Usage: smt2gxl <filename>\nsee -help for more options" in

let rec print_results = function
  | h::t -> 
     (function | SS.Atom x -> print_string x | SS.List y -> print_results y) h; print_results t
  | [] -> () in

let parse_result file = 
  let results = SS.load_sexps file in
  let () = IFDEF DEBUG THEN 
		 print_endline (string_of_int (L.length results)); 
		 print_results [(L.nth results 1)]
		 ELSE () ENDIF in () in

let output_graph name graph_attrs = 
  (* These are the nodes from the allocation *)
  (* let graph_elements =  *)
  let graph = GXL.gxl_graph_make ~role:None ~edgeids:None ~hypergraph:None ~edgemode:GXL.Directed ~gxl_type:None ~attrs:graph_attrs 
				 ~elements:[] ~id:name in
  {GXL.graphs=[graph];GXL.xlink="http://www.w3.org/1999/xlink"} in

try
  let file_name = ref "" in
  let output = ref "" in
  let graph = ref "" in
  let processors = ref 1 in
  let speclist = Arg.align [
		     ("-o", Arg.Set_string output, " the name of the output gxl file, nothing generated if not given");
		     ("-i", Arg.Set_string graph, " the name of the input graph gxl file [required]");
		     ("-processors", Arg.Set_int processors, " # of processors [default: 1]");
		   ] in
  let () = Arg.parse speclist (fun x -> file_name := x) usage_msg in
  let () = if !file_name = "" then raise (File_Not_found !file_name) else () in
  let () = if !graph = "" then raise (File_Not_found !graph) else () in
  let pp = G.make () in
  let () = G.parse !graph pp in
  let gxl_element = G.get_document_element pp in
  let graphs = GXL.get_gxl_gxl_graph_list gxl_element in
  let graph_attrs = L.map GXL.get_typed_element_attr_list graphs in
  
  (* parsing the result from smt *)
  let () = parse_result !file_name in
  
  (* This is the parsing of the output and generation of the output graph *)
  if !output <> "" then
    let graph_id = GXL.get_typed_element_id (L.hd graphs) in
    let processor_attr =  GXL.gxl_attr_make  (GXL.gxl_atomic_value_make (GXL.gxl_int_make !processors)) "Processors" in
    let ochan = open_out !output in
    output_string ochan (Xml.to_string_fmt (GXL.gxl_gxl_to_xml (output_graph graph_id ((L.hd graph_attrs) @ [processor_attr]))));
    flush ochan; (*flush everything down*)
    close_out ochan
  
with
| End_of_file -> exit 0
| Sys_error  _ 
| File_Not_found _ -> Arg.usage [] usage_msg
| _ as s -> raise s

