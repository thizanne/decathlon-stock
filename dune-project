(lang dune 2.8)
(name decathlon-stock)
(generate_opam_files true)

(package
 (name decathlon-stock)
 (synopsis "Check Decathlon online stock availability")
 (maintainers "Thibault Suzanne <thi.suzanne@gmail.com>")
 (authors "Thibault Suzanne <thi.suzanne@gmail.com>")
 (homepage "https://github.com/thizanne/decathlon-stock/")
 (bug_reports "https://github.com/thizanne/decathlon-stock/issues")
 (depends
  core_kernel
  cmdliner
  async_smtp
  cohttp-lwt-unix
  cohttp-lwt
  lambdasoup
  fmt
  ppx_let
  yojson
  )
 )
