(** Prepare a "prep" tree for odocmkgen:
    - Add listing packages (universes, packages in universes, versions of packages)
    - Add default package page if there is not already one
    - TODO: Add some informations to user's package pages ?
      (dependencies to other packages, list of modules and pages)
    *)

let ( / ) = Fpath.( / )

let fpf = Printf.fprintf

(* TODO: Take this as CLI argument. *)
let top_path = Prep.top_path

module Deps : sig
  type t
  (** Mutable data-structure storing dependencies. *)

  val create : unit -> t

  val add : t -> target:Fpath.t -> Fpath.t list -> unit
  (** Paths are internally normalized. [target] must be a path to a file, deps
      can be paths to files or directories. *)

  val fold : (Fpath.t -> Fpath.t list -> 'a -> 'a) -> t -> 'a -> 'a
end = struct
  module Tbl = Hashtbl.Make (struct
    type t = Fpath.t

    let equal = Fpath.equal

    let hash = Hashtbl.hash
  end)

  type t = Fpath.t list Tbl.t

  let create () = Tbl.create 50

  let add tbl ~target extra_deps =
    let target = Fpath.normalize target in
    let deps = match Tbl.find_opt tbl target with Some d -> d | None -> [] in
    let extra_deps = List.rev_map Fpath.normalize extra_deps in
    Tbl.replace tbl target (List.rev_append extra_deps deps)

  let fold = Tbl.fold
end

let write_file p f =
  let out = open_out (Fpath.to_string p) in
  Format.eprintf "Create '%a'\n" Fpath.pp p;
  Fun.protect ~finally:(fun () -> close_out out) (fun () -> f out)

let index_page_of_dir d =
  let parent, base = Fpath.split_base d in
  Fpath.( // ) parent (Fpath.set_ext ".mld" base)

let is_dir (_, p) = Sys.is_directory (Fpath.to_string p)

let is_hidden s =
  let len = String.length s in
  let rec aux i =
    if i > len - 2 then false
    else if s.[i] = '_' && s.[i + 1] = '_' then true
    else aux (i + 1)
  in
  aux 0

(** Replace '-' by '_' and add the "page-" prefix. That's what [odocmkgen] is
    doing. *)
let page_name_of_string n =
  "page-" ^ String.concat "_" (String.split_on_char '-' n)

let module_name_of_string = String.capitalize_ascii

(** This is an approximation. *)
let module_name_of_path f =
  if Fpath.mem_ext [ ".cmti"; ".cmt"; ".cmi" ] f then
    Some (module_name_of_string (Fpath.basename (Fpath.rem_ext f)))
  else None

(** Search recursively for modules. *)
let rec modules_of_subpkg p =
  let get_module (_, p) =
    match module_name_of_path p with
    | Some m as m' when not (is_hidden m) -> m'
    | _ -> None
  in
  let dirs, files = List.partition is_dir (Util.list_dir p) in
  (* Remove duplicates due to several cm{ti,t,i} per modules. *)
  List.sort_uniq String.compare (List.filter_map get_module files)
  @ List.concat_map (fun (_, p') -> modules_of_subpkg p') dirs

let pp_childpages out =
  List.iter (fun p -> fpf out "- {!childpage:%s}\n" (page_name_of_string p))

let pp_childmodules out = List.iter (fpf out "- {!childmodule:%s}\n")

let gen_universes_list universes out =
  fpf out
    "{0 Universes}\n\
     These universes are for those packages that are compiled against an \
     alternative set of dependencies than those in the 'packages' hierarchy.\n";
  pp_childpages out universes;
  ()

let gen_universe_page universe_name packages out =
  fpf out
    "{0 Universe %s}\n\
     {1 Packages}\n\
     This dependency universe has been used to compile the following packages:\n"
    universe_name;
  pp_childpages out packages;
  ()

let gen_versions_list pkg_name versions out =
  fpf out "{0 Package '%s'}\n{1 Versions}\n" pkg_name;
  pp_childpages out versions;
  ()

let gen_package_page pkg_name version subpkgs out =
  let gen_subpkg (name, modules) =
    fpf out "{1 [%s]}\n" name;
    pp_childmodules out modules;
    ()
  in
  fpf out "{0 Package '%s' version %s}\n" pkg_name version;
  List.iter gen_subpkg subpkgs;
  ()

let assemble_package pkg_name version p =
  let subpkgs =
    (* This is not working in most cases.
       TODO: Better listing of sub packages. *)
    Util.list_dir (Fpath.( / ) p "lib")
    |> List.filter is_dir
    |> List.map (fun (s, p') -> (s, modules_of_subpkg p'))
  in
  (* TODO: Detect user's package pages (not yet copied by 'prep') *)
  write_file (index_page_of_dir p) (gen_package_page pkg_name version subpkgs)

let assemble_package_versions pkg_name p =
  let versions = List.filter is_dir (Util.list_dir p) in
  write_file (index_page_of_dir p)
    (gen_versions_list pkg_name (List.map fst versions));
  List.iter (fun (v, p') -> assemble_package pkg_name v p') versions

let assemble_universe universe_name p =
  let packages = List.filter is_dir (Util.list_dir p) in
  write_file (index_page_of_dir p)
    (gen_universe_page universe_name (List.map fst packages));
  List.iter
    (fun (pkg_name, p') -> assemble_package_versions pkg_name p')
    packages

let assemble_universes p =
  let universes = List.filter is_dir (Util.list_dir p) in
  write_file (index_page_of_dir p) (gen_universes_list (List.map fst universes));
  List.iter (fun (name, p') -> assemble_universe name p') universes

let query_comple_deps p =
  let process_line line =
    match Astring.String.cuts ~sep:" " line with
    | [ c_name; c_digest ] -> Some (c_name, c_digest)
    | _ -> None
  in
  Util.lines_of_process "odoc" [ "compile-deps"; Fpath.to_string p ]
  |> List.filter_map process_line

let rec list_dir_rec acc p =
  Util.list_dir p
  |> List.fold_left
       (fun acc ((_, f) as f') ->
         if is_dir f' then list_dir_rec acc f else f :: acc)
       acc

module DigestMap = Map.Make (Digest)

let compute_compile_deps paths =
  let deps_and_digest f =
    (* Query [odoc compile-deps]. *)
    if not (Fpath.mem_ext [ ".cmti"; ".cmt"; ".cmi" ] f) then None
    else
      let unit_name =
        String.capitalize_ascii Fpath.(to_string (rem_ext (base f)))
      in
      match
        List.partition (fun (name, _) -> name = unit_name) (query_comple_deps f)
      with
      | [ self ], deps -> Some (snd self, List.map snd deps)
      | _ ->
          Format.eprintf "Failed to find digest for self (%a)\n%!" Fpath.pp f;
          None
  in
  let deps_and_digests = List.rev_map deps_and_digest paths in
  let inputs_by_digest =
    List.fold_left2
      (fun acc f -> function Some (digest, _) -> DigestMap.add digest f acc
        | None -> acc)
      DigestMap.empty paths deps_and_digests
  in
  let find_dep dep = DigestMap.find_opt dep inputs_by_digest in
  let deps_tbl = Deps.create () in
  List.iter2
    (fun target -> function
      | Some (_, deps) ->
          Deps.add deps_tbl ~target (List.filter_map find_dep deps)
      | None -> ())
    paths deps_and_digests;
  deps_tbl

(** Temporary: Will be done by [prep] when collecting object files.
    Collect deps for every object files. *)
let assemble_dep_file deps dst =
  let print_relpath p =
    (* Make sure the paths are relative to [top_path]. *)
    match Fpath.relativize ~root:top_path p with
    | Some r -> Fpath.to_string r
    | None -> assert false
  in
  write_file dst (fun out ->
      Deps.fold
        (fun target deps () ->
          if deps = [] then ()
          else (
            fpf out "%s" (print_relpath target);
            List.iter (fun p -> fpf out " %s" (print_relpath p)) deps;
            fpf out "\n" ))
        deps ())

let run () =
  let deps = list_dir_rec [] top_path |> compute_compile_deps in
  assemble_universes (top_path / "universes");
  assemble_dep_file deps (top_path / "dep")
