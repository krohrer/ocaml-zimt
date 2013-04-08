C family foreign function interface generator - ocaml-zimt
======================================================================

Disclaimer: This is a toy projects and a work in progress. I basically try to use every feature OCaml 4.0 has to offer.

An OCaml foreign-function interface generator for the C language
family. The idea is to simplify the process of manually wrapping C
libraries by writing the interface in a DSL, which in turn generates
the necessary OCaml and C files. The DSL makes heavy use of
generalized algebraic data types.
