(* Prep *)

open Listm

(* This module prepares a directory structure for making documentation.

  We go through all the installed cm{t,ti,i}s and find which package/version they are from. We then
  query opam to find out their dependencies. Then we copy the files into the following structure:
  
  prep/universes/<hash>/<package>/<version>/ocaml/... 
  
  *)

type package_info = {
  package_opam : Opam.package;
  universe : Universe.t;
}

type source_info = {
  root : Fpath.t;  (** Root path in which this was found *)
  relpath : Fpath.t;  (** Path relative to [root] *)
  name : string;  (** 'Astring', 'page-index' *)
  package : package_info; (* Package in which this file lives ("astring") *)
}

let top_path = Fpath.v "prep"

(** Get info given a relative path (to [root]) to an object file (cmt, cmti or cmi). *)
let get_cm_info root package relpath =
  let _, lname = Fpath.split_base relpath in
  let name = String.capitalize (Fpath.to_string lname) in
  try [ { root; relpath; name; package } ] with _ -> []

(** Get info given a doc file (mld) *)
let get_doc_info root package relpath =
  let _, lname = Fpath.split_base relpath in
  let name = "page-" ^ Fpath.to_string lname in
  try [ { root; relpath; name; package } ] with _ -> []

(** Lower is better *)
let cm_file_preference = function
  | ".cmti" -> Some 1
  | ".cmt" -> Some 2
  | ".cmi" -> Some 3
  | _ -> None

(** Get cm* files out of a list of files.
    Given the choice between a cmti, a cmt and a cmi file, we chose them according to [cm_file_preference] above *)
let get_cm_files files =
  let rec skip f = function hd :: tl when f hd -> skip f tl | x -> x in
  (* Take the first of each group. *)
  let rec dedup acc = function
    | (base, _, p) :: tl ->
        let tl = skip (fun (base', _, _) -> base = base') tl in
        dedup (p :: acc) tl
    | [] -> acc
  in
  (* Sort files by their basename and preference, remove other files *)
  files
  >>= (fun p ->
        let without_ext, ext = Fpath.split_ext p in
        match cm_file_preference ext with
        | Some pref -> [ (Fpath.basename without_ext, pref, p) ]
        | None -> [])
  |> List.sort compare |> dedup []

(** A list of [source_info] for each files of a package.
    Keep only one of the corresponding .cmti, .cmt or .cmi for a module, in
    that order of preference. *)
let infos_of_package package_opam =
  let partition_by_kind (doc, lib) f =
    let segs = Fpath.segs f in
    match segs with
    | "lib" :: _
      when not (List.mem ".private" segs || List.mem ".coq-native" segs) ->
        (doc, f :: lib)
    | "doc" :: _ when Inputs.has_ext [ ".mld" ] f ->
        (* TODO: keep other doc files (may be .md, .ml or others). *)
        (f :: doc, lib)
    | _ -> (doc, lib)
  in
  let package_name = package_opam.Opam.name in
  let doc, lib =
    List.fold_left partition_by_kind ([], []) (Opam.pkg_contents package_name)
  in
  let lib = get_cm_files lib in
  let prefix = Opam.prefix () |> Fpath.v in
  let package =
    let _, universe = Universe.Current.dep_universe package_name in
    Format.eprintf "%a: universe=%a\n%!" Opam.pp_package package_opam
      Universe.pp universe;
    { package_opam; universe }
  in
  List.flatten
    ( List.map (get_cm_info prefix package) lib
    @ List.map (get_doc_info prefix package) doc )

let run whitelist _roots =
  let packages = Opam.all_opam_packages () in
  let packages = List.filter (fun pkg -> pkg.Opam.name <> "ocaml-secondary-compiler") packages in
  let infos = List.flatten (List.map infos_of_package packages) in

  let infos =
    if List.length whitelist > 0
    then List.filter (fun info -> List.mem info.package.package_opam.name whitelist) infos
    else infos
  in
  let infos =
    List.filter (fun info -> try ignore (Universe.Current.dep_universe info.package.package_opam.name); true with _ -> Format.eprintf "pruning %a\n%!" Fpath.pp (Fpath.append info.root info.relpath); false) infos
  in
  let copy info =
    let { package_opam; universe } = info.package in
    let v_str = Astring.String.cuts ~sep:"." package_opam.version in
    let v_str = String.concat "_" v_str in
    let src = Fpath.append info.root info.relpath in
    let dest = Fpath.(top_path / "universes" / universe.id / package_opam.name / v_str // info.relpath ) in
    Util.mkdir_p (Fpath.parent dest);
    Util.cp src dest
  in
  List.iter copy infos;
  Universe.Current.save top_path
