open GxlDocument

let pp = GxlDocument.make ()

(* Parse an attributed file *)
let () = GxlDocument.parse "../Fork_Join_Nodes_30_CCR_1.99_WeightType_Random.gxl" pp
let () = GxlDocument.write "/tmp/tutu.gxl" pp
