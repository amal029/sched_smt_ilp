module L = Batteries.List
module S = Batteries.String
module StringSet = Batteries.Set
module LL = Batteries.LazyList
module E = Batteries.Enum
module H = Batteries.Hashtbl
module SS = Sexplib.Sexp
module G = GxlDocument.GxlDocument

open Sexplib.Std
open Pretty

exception File_Not_found of string
exception Internal_error of string

type t = 
  | Node of string * float option * bool option
  | Noop
with sexp

let (|>) x f = f x
let usage_msg = "Usage: smt2gxl <filename>\nsee -help for more options"

let get_val = function
  | SS.Atom x -> x
  | _ -> raise (Internal_error "Expected Real or Bool")

let rec get_results = function
  | h::t -> 
     (function 
       | SS.Atom x -> 
	  if (x = "define-fun")  then
	    begin
	      let name = (L.nth t 0) |> get_val in
	      let value = (L.nth t 3) |> get_val in
	      if (get_val (L.nth t 2) = "Real") then
		[Node (name, Some (float_of_string value), None)]
	      else if (get_val (L.nth t 2) = "Bool") then
		[Node (name, None, Some (bool_of_string value))]
	      else [Noop]
	    end
	  else [Noop]
       | SS.List y -> get_results y) h @ (get_results t)
  | [] -> []

let get_processor_alloc n = function
  | Node (x,_,_) -> 
     let name = L.nth (Str.split (Str.regexp "_") x) 1 in
     name = n
  | Noop -> false 
       

let parse_result file = 
  let results = SS.load_sexps file in
  let results = get_results [(L.nth results 1)] in
  let results = L.filter (function | Node _ -> true | _ -> false) results 
		|> L.filter (function | Node (y,_,x)-> (match x with | Some x -> x | None -> not (S.contains y '!')) | _ -> false) in
  let () = IFDEF DEBUG THEN SS.output_hum stdout (sexp_of_list sexp_of_t results) ELSE () ENDIF in
  let rresults = L.filter (function Node (_,Some _,_) -> true | _ -> false) results in
  let rresults = L.filter (function Node (x,_,_) -> x <> "M") rresults in
  let bresults = L.filter (function Node (_,_,Some _) -> true | _ -> false) results in
  let bresults = L.filter (function Node (x,_,_) -> x <> "M") bresults in
  (* Now build the elements for the graph *)
  L.map (function 
	  | Node (x,y,z) ->
	     let () = IFDEF DEBUG THEN print_endline ("Name: " ^ x ^ " getting name") ELSE () ENDIF in
	     let name = L.nth (Str.split (Str.regexp "_") x) 1 in
	     let () = IFDEF DEBUG THEN print_endline ("Name: " ^ name ^ " getting alloc") ELSE () ENDIF in
	     let alloc = L.filter (get_processor_alloc name) bresults in
	     let alloc = 
	       if alloc <> [] then L.hd alloc 
	       else raise (Internal_error ("Cannot find the processor allocation for node: " ^ name)) in
	     let alloc = match alloc with | Node (x,_,_) -> L.nth (Str.split (Str.regexp "_") x) 2 |> int_of_string in
	     let () = IFDEF DEBUG THEN print_endline ("ALLOC: " ^ (string_of_int alloc)) ELSE () ENDIF in
	     let stime = match y with Some x -> x | None -> raise (Internal_error ("Cannot get the start time for: "^ name)) in 
	     let () = IFDEF DEBUG THEN print_endline ("STIME: " ^ (string_of_float stime)) ELSE () ENDIF in
	     let stime = GXL.gxl_attr_make (GXL.gxl_atomic_value_make (GXL.gxl_float_make stime)) "Start time" in
	     let processor = GXL.gxl_attr_make (GXL.gxl_atomic_value_make(GXL.gxl_int_make alloc)) "Processor" in
	     GXL.gxl_node_make ~gxl_type:None ~graphs:[] ~attrs:[stime;processor] ~id:name
	  | Noop -> raise (Internal_error "Something has gone horribly wrong, I am dying")
	) rresults
	
  

let output_graph nodes name graph_attrs = 
  (* These are the nodes from the allocation *)
  (* let graph_elements =  *)
  let graph = GXL.gxl_graph_make ~role:None ~edgeids:None ~hypergraph:None ~edgemode:GXL.Directed ~gxl_type:None ~attrs:graph_attrs 
				 ~elements:nodes ~id:name in
  {GXL.graphs=[graph];GXL.xlink="http://www.w3.org/1999/xlink"}

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
  let nodes = parse_result !file_name in
  let () = IFDEF DEBUG THEN print_endline "results parsed" ELSE () ENDIF in
  
  (* This is the parsing of the output and generation of the output graph *)
  if !output <> "" then
    let graph_id = GXL.get_typed_element_id (L.hd graphs) in
    let processor_attr =  GXL.gxl_attr_make  (GXL.gxl_atomic_value_make (GXL.gxl_int_make !processors)) "Processors" in
    let ochan = open_out !output in
    output_string ochan (Xml.to_string_fmt (GXL.gxl_gxl_to_xml (output_graph nodes graph_id ((L.hd graph_attrs) @ [processor_attr]))));
    flush ochan; (*flush everything down*)
    close_out ochan
  
with
| End_of_file -> exit 0
| Sys_error  _ 
| File_Not_found _ -> Arg.usage [] usage_msg
| _ as s -> raise s

