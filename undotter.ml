(* ******************************************************************* *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique. All rights reserved. This file is distributed under   *)
(*  the BSD 2-Clause licence.                                          *)
(*                                                                     *)
(* 2021 T. Bourke                                                      *)
(* ******************************************************************* *)

(** Versioning *)

let version =
  match Build_info.V1.version () with
  | None -> "unknown"
  | Some v -> Build_info.V1.Version.to_string v

let pp_version_string ppf () =
  Format.fprintf ppf "Undotter, version %s" version

let show_version () =
  Format.printf "%a@," pp_version_string ()

let usage_msg = "usage: undotter [options] [files]"

(** Global settings *)

let verbose = ref 0

let output_prefix = ref "cycles"

let ideal_size = ref 25

let group_edges_threshold = ref 3

let do_relinking = ref true

let cluster_external_nodes = ref false

(** Utilities *)

let printf = Format.printf
let eprintf = Format.printf

let with_out_channel out f =
  let r = try f out with e -> (close_out out; raise e) in
  close_out out;
  r

let with_added_formatter f out = f (Format.formatter_of_out_channel out)

let with_out_file fname f = with_out_channel (Stdlib.open_out fname) f

let with_out_formatter fname f =
  with_out_channel (Stdlib.open_out fname) (with_added_formatter f)

let pp_semi p ()  = Format.(pp_print_char p ';'; pp_print_space p ())
let pp_comma p ()  = Format.(pp_print_char p ','; pp_print_space p ())

let prefer_right _ vl vr =
  match vl, vr with
  | _, Some x -> Some x
  | vr, _ -> vr

(** Graphs *)

module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
  (struct
    type t = Graph.Dot_ast.node_id
    let compare = Stdlib.compare
    let equal x y = Stdlib.compare x y = 0
    let hash = Hashtbl.hash
  end)
  (struct
    type t = Graph.Dot_ast.attr list
    let compare = Stdlib.compare
    let default = []
   end)

module GB = Graph.Builder.P(G)
module Components = Graph.Components.Make(G)
module Emap = Graph.Gmap.Edge(G)(struct include GB.G include GB end)

module AttrOrdered =
  struct
    type t = Graph.Dot_ast.id * Graph.Dot_ast.id option
    let compare = Stdlib.compare
  end

let filter_attrs keep =
  let f atts = match List.filter keep atts with
               | [] -> None
               | xs -> Some xs
  in
  List.filter_map f

(* Sets of attributes *)
module AttrSet = struct (* {{{ *)
  include Set.Make(AttrOrdered)

  let of_attrlist = List.fold_left (List.fold_left (fun s nv -> add nv s)) empty

  let to_attrlist s = [elements s]

  let partition_attrs keep attss = List.(split (map (partition keep) attss))

end (* }}} *)

(* Map sets of attributes to arbitrary values. *)
module AttrSetMap = struct (* {{{ *)
  include Map.Make (struct
      type t = G.vertex * AttrSet.t * G.vertex

      let compare (src1, s1, dst1) (src2, s2, dst2) =
        match G.V.compare src1 src2 with
        | 0 -> (match G.V.compare dst1 dst2 with
                | 0 -> AttrSet.(compare s1 s2)
                | r -> r)
        | r -> r
     end)

  let add_with_cons k v =
    update k (function None -> Some [v] | Some vs -> Some (v::vs))
end (* }}} *)

(** Graphviz interface *)

module GV = struct (* {{{ *)

  module Dot_ast = Graph.Dot_ast

  let string_of_id = function
    | Dot_ast.Ident s | Dot_ast.Number s | Dot_ast.String s -> s
    | Dot_ast.Html s -> "<" ^ s ^ ">"

  module NodeMap = Map.Make
      (struct
        type t = Dot_ast.node_id
        let compare = Stdlib.compare
      end)

  module EdgeMap = Map.Make
      (struct
        type t = Dot_ast.node_id * Dot_ast.node_id
        let compare = Stdlib.compare
      end)

  type node_attrs = Dot_ast.attr list NodeMap.t
  let no_node_attrs = NodeMap.empty

  (* hack to capture node attributes *)
  let attr_map = Hashtbl.create 1000

  module Parse = Graph.Dot.Parse
                    (struct
                      module G = G
                      include G
                      let copy x = x
                      let empty () = empty
                    end)
                    (struct
                      let node n attr =
                        Hashtbl.add attr_map n attr; n
                      let edge e = e
                     end)

  let parse path =
    let g = Parse.parse path in
    let attrs = Hashtbl.fold NodeMap.add attr_map NodeMap.empty in
    Hashtbl.reset attr_map;
    (g, attrs)

  let make_cluster_edge src dst =
    (src, [[Dot_ast.(Ident "style", Some (Ident "invis"))]], dst)

  let make_id n = (Dot_ast.Ident n, None)

  let make_attr n v =
    [(Dot_ast.Ident n, Some (Dot_ast.String v))]

  let pp_id ppf = function
    | Dot_ast.Ident s -> Format.pp_print_string ppf s
    | Dot_ast.Number s -> Format.pp_print_string ppf s
    | Dot_ast.String s -> Format.fprintf ppf "\"%s\"" s
    | Dot_ast.Html s -> Format.fprintf ppf "<%s>" s

  let string_of_compass_point = function
    | Dot_ast.N  -> ":n"
    | Dot_ast.Ne -> ":ne"
    | Dot_ast.E  -> ":e"
    | Dot_ast.Se -> ":se"
    | Dot_ast.S  -> ":s"
    | Dot_ast.Sw -> ":sw"
    | Dot_ast.W  -> ":w"
    | Dot_ast.Nw -> ":nw"

  let pp_compass_point ppf cpt =
    Format.fprintf ppf ":%s" (string_of_compass_point cpt)

  let string_of_port = function
    | Dot_ast.PortId (id, None) -> string_of_id id
    | Dot_ast.PortId (id, Some cpt) ->
        string_of_id id ^ string_of_compass_point cpt
    | Dot_ast.PortC cpt -> string_of_compass_point cpt

  let pp_port ppf = function
    | Dot_ast.PortId (id, ocpt) ->
        pp_id ppf id; Option.iter (pp_compass_point ppf) ocpt
    | Dot_ast.PortC cpt ->
        pp_compass_point ppf cpt

  let pp_attr ppf (n, ov) =
    Format.open_hbox ();
    pp_id ppf n;
    Option.iter (fun v -> Format.fprintf ppf "=%a" pp_id v) ov;
    Format.close_box ()

  let pp_attrs ppf = function
    | [] -> ()
    | attrs -> Format.(fprintf ppf "@[<hv 2>[%a]@]@,"
                          (pp_print_list ~pp_sep:pp_comma pp_attr) attrs)

  let pp_attrs_list ppf attrs_l =
    Format.open_hvbox 0;
    List.iter (pp_attrs ppf) attrs_l;
    Format.close_box ()

  let string_of_node_id (id, op) =
    match op with
    | None -> string_of_id id
    | Some p -> string_of_id id ^ string_of_port p

  let pp_node_id ppf (x, op) =
    pp_id ppf x;
    (match op with
     | None -> ()
     | Some p -> Format.(pp_print_char ppf ':'; pp_port ppf p))

  let print_vertex node_attrs ppf v =
    pp_node_id ppf v;
    Option.iter (pp_attrs_list ppf) (NodeMap.find_opt v node_attrs);
    pp_semi ppf ()

  let print_edge ppf e =
    Format.fprintf ppf "%a -> %a %a@;"
      pp_node_id (G.E.src e)
      pp_node_id (G.E.dst e)
      pp_attrs_list (G.E.label e)

  let print_cluster ppf (n, vs) =
    Format.(fprintf ppf
              {|subgraph cluster_%s @[<hv 1>{
label = %s
fontname = ""
color = "lightgray"
fontcolor = "lightgray"
@;%a}@]@;|} n n
                (pp_print_list ~pp_sep:pp_semi pp_id) vs)

  let print_title ppf title =
    Format.fprintf ppf
      {|labelloc="t"@;label="%s"@;fontname="bold"@;|} title

  let print_dot ?title ?(clusters=[]) ?(node_attrs=NodeMap.empty) ppf g =
    let open Format in
    fprintf ppf "digraph@;@[<v 2>{@;";
    Option.iter (print_title ppf) title;
    G.iter_vertex (print_vertex node_attrs ppf) g;
    G.iter_edges_e (print_edge ppf) g;
    List.iter (print_cluster ppf) clusters;
    fprintf ppf "@;<0 -2>}@]@."

end (* }}} *)

(* TODO: replace when available in ocamlgraph *)
module BiConnectivity (G: Graph.Sig.G) :
  sig
    module S : Set.S with type elt = G.vertex

    val strong_articulation_points : G.t -> G.vertex list

    val sstrong_articulation_points : G.t -> S.t
  end
  =
struct (* {{{ *)

  module Choose = Graph.Oper.Choose(G)
  module Dom = Graph.Dominator.Make(G)
  module RDom = Graph.Dominator.Make(
      struct
        type t = G.t
        module V = G.V
        let pred = G.succ
        let succ = G.pred
        let fold_vertex = G.fold_vertex
        let iter_vertex = G.iter_vertex
        let iter_succ = G.iter_pred
        let nb_vertex = G.nb_vertex
      end)

  module S = Dom.S

  let dom_tree_to_snontrivial_dom v dt =
    let rec f rs = function
      | [] -> rs
      | x::xs ->
          (match dt x with
           | [] -> f rs xs
           | ys -> f (Dom.S.add x rs) (List.rev_append ys xs))
    in
    f Dom.S.empty (dt v)

  let sstrong_articulation_points g =
    let s = Choose.choose_vertex g in
    let module SCC = Graph.Components.Make (struct
        include G
        let iter_vertex f =
          G.iter_vertex (fun v -> if not (V.equal s v) then f v)
        let iter_succ f =
          G.iter_succ (fun v -> if not (V.equal s v) then f v)
      end)
    in
    let s_is_sap = fst (SCC.scc g) > 1 in
    let dt_s = Dom.(idom_to_dom_tree g (compute_idom g s)) in
    let d_s = dom_tree_to_snontrivial_dom s dt_s in
    let dtr_s = RDom.(idom_to_dom_tree g (compute_idom g s)) in
    let dr_s = dom_tree_to_snontrivial_dom s dtr_s in
    let d = Dom.S.union d_s dr_s in
    if s_is_sap then Dom.S.add s d else d

  let strong_articulation_points g = S.elements (sstrong_articulation_points g)

end (* }}} *)

module C = BiConnectivity(G)
module S = C.S

let add_with_union k v =
  GV.NodeMap.update k (function
      | None -> Some (S.singleton v)
      | Some s -> Some (S.add v s))

(** Core algorithms *)

let vertexes g = G.fold_vertex S.add g S.empty

type cluster = string * S.t

let gvcluster (n, s) = (n, S.fold (fun x xs -> fst x :: xs) s [])

let pp_graph_info ppf g =
  Format.fprintf ppf "%d nodes/%d edges" (G.nb_vertex g) (G.nb_edges g)

(* project out the subgraph containing just the vertices in vs, i.e. G[vs]. *)
let subgraph g vs =
  let in_graph e =
    if S.mem (G.E.src e) vs && S.mem (G.E.dst e) vs then Some e else None
  in
  Emap.filter_map in_graph g

type chunk_info = {
  cid      : G.vertex;
  chunk    : G.t;
  cattrs   : GV.node_attrs;
  clusters : (string * GV.Dot_ast.id list) list;
}

type state = {
  chunk_prefix : string;
  next_chunk   : int;
  chunks       : chunk_info list;
  node_attrs   : GV.node_attrs;
  node_chunks  : G.vertex GV.NodeMap.t; (* map nodes to their chunks *)
}

let init_state chunk_prefix node_attrs = {
  chunk_prefix;
  next_chunk  = 0;
  chunks      = [];
  node_attrs;
  node_chunks = GV.NodeMap.empty;
}

let add_attrs attrmap id attrs =
  let existing = Option.value (GV.NodeMap.find_opt id attrmap) ~default:[] in
  GV.NodeMap.add id (List.rev_append attrs existing) attrmap

let add_node_attrs ({ node_attrs; _ } as state) id attrs =
  { state with node_attrs = add_attrs node_attrs id attrs }

let add_chunk chunk ({ chunk_prefix; next_chunk; chunks;
                       node_attrs; node_chunks; _ } as state) =
  let cid = GV.make_id (Printf.sprintf "%s%03d" chunk_prefix next_chunk) in
  let node_chunks' = G.fold_vertex (fun v -> GV.NodeMap.add v cid)
                        chunk node_chunks in
  (cid, { state with
            next_chunk = next_chunk + 1;
            chunks =
              { cid; chunk; cattrs = GV.no_node_attrs; clusters = [] } :: chunks;
            node_attrs = GV.NodeMap.add cid [GV.make_attr "shape" "box"] node_attrs;
            node_chunks = node_chunks';
        })

(* Count the number of edges in scc that go into or out of the subset vertexes
   in vs. *)
let nb_inter_edges scc vs =
  let svs = S.of_list vs in
  let add_edges s v = s
    |> G.fold_succ (fun v' s -> if S.mem v' svs then s else s + 1) scc v
    |> G.fold_pred (fun v' s -> if S.mem v' svs then s else s + 1) scc v
  in
  List.fold_left add_edges 0 vs

(* Split a strongly connected component at a strong articulation point to
   produce a set of new storngly connected components, filter out the trivial
   ones, and assign a score relative to the ideal size. *)
let split_at_sap scc sap =
  let sccvs, crumbs =
    GB.remove_vertex scc sap
    |> Components.scc_list
    |> List.partition (fun x -> List.length x > 1)
  in
  let quality s xs = s + List.length xs + nb_inter_edges scc xs/2 in
  match List.length sccvs with
  | 0 -> None
  | len ->
    let avg = List.(fold_left quality 0 sccvs) / len in
    Some (abs (!ideal_size - (avg + List.length crumbs)), sccvs)

(* Split a strongly connected component at one of the strong articulation
   points in the given set, trying to maximize the relative quality of the
   split (lower is better). *)
let choose_best scc saps =
  let f sap b =
    if G.mem_vertex scc sap then
      (match split_at_sap scc sap with
       | None -> b
       | Some (q, sccvs) ->
           (match b with
            | Some (bq, _) when bq <= q -> b
            | _ -> Some (q, sccvs)))
    else b
  in
  S.fold f saps None

(* Replace the chunk in g by a representative node. *)
let zap_chunk { cid = repv; chunk; _ } g0 =
  let zap_edge (src, l, dst) g =
    if G.mem_vertex chunk dst then
      (if G.mem_vertex chunk src then g
       else G.add_edge_e g (src, l, repv))
    else if G.mem_vertex chunk src then G.add_edge_e g (repv, l, dst)
    else g
  in
  let g1 = G.fold_edges_e zap_edge g0 g0 in
  let g2 = G.fold_vertex (fun v g -> G.remove_vertex g v) chunk g1 in
  g2

(** Tries to recursively break a strongly connected component into smaller
    chunks to make it easier to visualize cycles in the graph. If possible
    each chunk has less vertexes than the given bound. The returned
    boolean indicates whether the orginal graph contained any strong
    articulation points (if not it is returned unchanged). *)
let rec smash ?(depth=0) state0 g0 =
  let istr' = String.init (2*depth) (fun i -> if i mod 2 = 0 then '|' else ' ') in
  let istr = istr' ^ "+" in
  if G.nb_vertex g0 <= !ideal_size then
    (if !verbose > 1 then printf "%s- %d node(s) <= %d@."
                            istr (G.nb_vertex g0) !ideal_size;
     snd (add_chunk g0 state0))
  else
    let saps = C.sstrong_articulation_points g0 in
    if S.is_empty saps then
      (if !verbose > 1 then printf "%s- %d node(s) and NO saps@."
                              istr (G.nb_vertex g0);
       snd (add_chunk g0 state0))
    else begin
      if !verbose > 1 then printf "%s- smashing %d node(s) with %d sap(s)@."
                              istr (G.nb_vertex g0) (S.cardinal saps);
      match choose_best g0 saps with
      | None ->
          if !verbose > 1 then printf "%s- no good saps@." istr;
          snd (add_chunk g0 state0)
      | Some (_, sccvs) ->
        let chunks = List.map (fun k -> subgraph g0 (S.of_list k)) sccvs in
        if !verbose > 1 then
            printf "%s- %d chunk(s): @[<hv 2>%a@]@." istr (List.length chunks)
              Format.(pp_print_list ~pp_sep:pp_comma pp_graph_info) chunks;
        let state1 = List.fold_left (smash ~depth:(depth+1)) state0 chunks in
        let g1 = List.fold_right zap_chunk state1.chunks g0 in
        if G.nb_vertex g1 <= !ideal_size then begin
          if depth > 0 then state1
          else begin
            if !verbose > 1 then printf "%s- returning %a@." istr pp_graph_info g1;
            snd (add_chunk g1 state1)
          end
        end else begin
          if !verbose > 1 then printf "%s- resmashing %a@." istr pp_graph_info g1;
          smash ~depth state1 g1
        end
  end

(* Group together edges that only differ in their labels into a single edge
   with the shared attributes and a new label that is the concatenation of the
   original ones. *)
let group_like_edges g =
  let module ASM = AttrSetMap in
  let module AS = AttrSet in
  let string_of_value (_, v) =Option.map GV.string_of_id v in
  let concat_values vs = List.(concat (map (filter_map string_of_value) vs)) in
  let collect ((s, l, d) as e) =
    let ls, nls =
      AS.partition_attrs (fun (n, _) -> n = Graph.Dot_ast.Ident "label") l in
    ASM.add_with_cons (s, AS.of_attrlist nls, d) (e, concat_values ls)
  in
  let em = G.fold_edges_e collect g ASM.empty in
  let remove (g, rs) (e, ls) = (G.remove_edge_e g e, ls @ rs) in
  let group (src, attrs, dst) els g =
    if List.length els < !group_edges_threshold then g else
      let g', ls = List.fold_left remove (g, []) els in
      let l = GV.make_attr "label" (String.concat "\n+ " ls)
                :: AS.to_attrlist attrs in
      G.add_edge_e g' (src, l, dst)
  in
  ASM.fold group em g

(* Apply group_like_edges in a state. *)
let group_chunk_edges ({ chunks; _ } as state) =
  let f ({ cid; chunk; _ } as cinfo) =
    let chunk' = group_like_edges chunk in
    if !verbose > 1 && G.nb_edges chunk <> G.nb_edges chunk'
    then printf "group edges in %a: %a => %a@."
           GV.pp_node_id cid pp_graph_info chunk pp_graph_info chunk';
    { cinfo with chunk = chunk' }
  in
  { state with chunks = List.map f chunks }

let add_cluster_to_label (cl, _) (s, p) =
  (GV.Dot_ast.String (Printf.sprintf "%s\n(%s)"
                        (GV.string_of_id s)
                        (GV.string_of_id cl)), p)

(* Add the edges that come into or go out of a chunk. *)
let relink_chunk node_chunks g chunk_i =
  let relink_attrs = [
      GV.make_attr "color" "lightgray" @
      GV.make_attr "fontcolor" "lightgray" @
      GV.make_attr "fontsize" "12";
  ] in
  let not_in_new_attrs (n, _) =
    List.for_all (List.for_all (fun (n', _) -> n <> n')) relink_attrs in
  let make_grey attrs = relink_attrs @ filter_attrs not_in_new_attrs attrs in
  let with_cluster v =
    if !cluster_external_nodes then v else
      match GV.NodeMap.find_opt v node_chunks with
      | Some cid -> add_cluster_to_label cid v
      | None -> v
  in
  let f (src, l, dst) (chunk, ns, clustermap) =
    let src_in_chunk = G.mem_vertex chunk_i src in
    let dst_in_chunk = G.mem_vertex chunk_i dst in
    if src_in_chunk = dst_in_chunk then (chunk, ns, clustermap) else
      let src' = if src_in_chunk then src else with_cluster src in
      let dst' = if dst_in_chunk then dst else with_cluster dst in
      let v, v' = if src_in_chunk then dst, dst' else src, src' in
      (G.add_edge_e chunk (src', make_grey l, dst'),
       S.add v' ns,
       match GV.NodeMap.find_opt v node_chunks with
       | Some cid -> add_with_union cid v' clustermap
       | None -> clustermap)
  in
  G.fold_edges_e f g (chunk_i, S.empty, GV.NodeMap.empty)

let to_clusters clustermap =
  let felement (v, _) vs = v :: vs in
  let fcluster k v cs =
    (GV.string_of_node_id k, S.fold felement v []) :: cs
  in
  GV.NodeMap.fold fcluster clustermap []

(* Apply relink_chunk in a state. *)
let relink g ({ chunks; _} as state0) =
  let grayattr = [
    GV.make_attr "color" "lightgray" @
    GV.make_attr "fontcolor" "lightgray" @
    GV.make_attr "shape" "plain" @
    GV.make_attr "fontsize" "12"
  ] in
  let f ({ chunks; node_chunks; _ } as state) ({ chunk; cattrs; _ } as cinfo) =
    let chunk', s, clustermap = relink_chunk node_chunks g chunk in
    let cattrs' = S.fold (fun v cattrs -> add_attrs cattrs v grayattr) s cattrs in
    let clusters' = if !cluster_external_nodes then to_clusters clustermap else [] in
    { state with chunks = { cinfo with chunk = chunk';
                                       cattrs = cattrs';
                                       clusters = clusters' } :: chunks; }
  in
  List.fold_left f { state0 with chunks = [] } chunks

(** Main functions *)

let write_chunk dstpath global_attrs { cid; chunk; cattrs; clusters } =
    let title = GV.string_of_node_id cid in
    let fname = Filename.concat dstpath (title ^ ".dot") in
    let node_attrs = GV.NodeMap.merge prefer_right global_attrs cattrs in
    if !verbose > 0 then printf "%s: %a@," fname pp_graph_info chunk;
    with_out_formatter fname (fun ppf -> GV.print_dot
                                 ~title ~clusters ~node_attrs ppf chunk)

let only_if condition f x = if condition then f x else x

let unknot_file srcpath =
  let g, node_attrs = GV.parse srcpath in
  let dstpath = Filename.dirname !output_prefix in
  let chunk_prefix = Filename.basename !output_prefix in
  let { chunks; node_attrs; _ } =
    smash (init_state chunk_prefix node_attrs) g
    |> group_chunk_edges
    |> only_if !do_relinking (relink g)
  in
  Format.open_vbox 0;
  List.iter (write_chunk dstpath node_attrs) chunks;
  Format.close_box ()

let _ =
  begin try
    Format.open_vbox 0;
    Arg.parse
    [
        "--version",
          Arg.Unit show_version,
          " Show the version number";

        "-v",
          Arg.Unit (fun () -> Printexc.record_backtrace true;
                              incr verbose),
          " Activate verbose mode";

        "--output-prefix",
          Arg.Set_string output_prefix,
          "<path-prefix> Set the path and prefix for output files";

        "--ideal-size",
          Arg.Set_int ideal_size,
          "<int> Set the ideal number of nodes per graph";

        "--group-edges-threshold",
          Arg.Set_int group_edges_threshold,
          "<int> Determines when edges are grouped together";

        "--no-external-edges",
          Arg.Clear do_relinking,
          " Do not show the links between clusters.";

        "--cluster-external-nodes",
          Arg.Set cluster_external_nodes,
          " Group the external nodes by cluster.";
    ]
    unknot_file usage_msg
  with Failure e ->
    (eprintf "undotter failed:@ %s@," e;
     if !verbose > 0 then eprintf "%s@," (Printexc.get_backtrace ()))
  end;
  Format.close_box ();
  Format.print_flush ()

