(lang dune 3.12)

(name spectator)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(package
 (name spectator)
 (allow_empty)
 (depends ocaml dune reactiveData promise_jsoo yojson))
