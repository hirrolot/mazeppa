(lang dune 3.14)

(using menhir 3.0)

(name mazeppa)

(version 0.5.2)

(generate_opam_files true)

(source
 (github hirrolot/mazeppa))

(authors "hirrolot <hirrolot@gmail.com>")

(maintainers "hirrolot <hirrolot@gmail.com>")

(license MIT)

(documentation https://github.com/hirrolot/mazeppa)

(package
 (name mazeppa)
 (synopsis "A modern supercompiler for call-by-value functional languages")
 (depends
  (ocaml
   (>= 4.14))
  dune
  pprint
  (checked_oint
   (= 1.0.0))
  ppx_deriving
  ppx_string_interpolation
  ppx_yojson_conv
  (ppx_blob
   (>= 0.9.0))
  bisect_ppx
  (menhir
   (>= 20180905))
  (spectrum
   (>= 0.6.0))
  clap
  (alcotest
   (>= 1.7.0))))
