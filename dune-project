(lang dune 3.13)

(name coap)

(generate_opam_files true)

(source
 (github hyper-systems/ocaml-coap))

(authors "Rizo I. <rizo@odis.io>")

(maintainers "Rizo I. <rizo@odis.io>" "Leo Soares <leojrfs@gmail.com>")

(license ISC)

(documentation https://hyper-systems.github.io/ocaml-coap)

(package
 (name coap)
 (synopsis "OCaml CoAP core library")
 (description
  "Constrained Application Protocol (RFC 7252) implementation for OCaml.")
 (depends
  (ocaml
   (>= 4.08.0))
  dune
  (coap-core
   (= :version))
  (coap-server-lwt
   (= :version))
  (fmt
   (>= 0.9.0))
  (cstruct
   (>= 6.2.0))
  (stdint
   (>= 0.7.2))
  (lwt
   (>= 5.7.0))
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat
   (and
    :with-dev-setup
    (>= 0.26.1)))
  (odoc
   (and
    :with-doc
    (>= 2.4.1)))))

(package
 (name coap-core)
 (synopsis "OCaml CoAP core library")
 (description
  "\nCore CoAP parser and type definitions.\n\nThis library should not be used directly by applications. Please install the\n`coap` package to start using a CoAP server or client.\n")
 (depends
  (ocaml
   (>= 4.08.0))
  dune
  (fmt
   (>= 0.9.0))
  (cstruct
   (>= 6.2.0))
  base-bigarray
  (stdint
   (>= 0.7.2))
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat
   (and
    :with-dev-setup
    (>= 0.26.1)))
  (odoc
   (and
    :with-doc
    (>= 2.4.1)))))

(package
 (name coap-server-lwt)
 (synopsis "OCaml CoAP core library")
 (description
  "Constrained Application Protocol (RFC 7252) implementation for OCaml.")
 (depends
  (ocaml
   (>= 4.08.0))
  dune
  (coap-core
   (= :version))
  (fmt
   (>= 0.9.0))
  (cstruct
   (>= 6.2.0))
  (cstruct-lwt
   (>= 6.2.0))
  (stdint
   (>= 0.7.2))
  (lwt
   (>= 5.7.0))
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat
   (and
    :with-dev-setup
    (>= 0.26.1)))
  (odoc
   (and
    :with-doc
    (>= 2.4.1)))))
