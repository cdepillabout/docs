# let
#   pkgs = import <nixpkgs> { };

# in
#   { what = pkgs.ocaml-ng.ocamlPackages_4_05.ocaml ;
#     hello = pkgs.hello ; 
#   }

{ pkgs? (import <nixpkgs> {}) }:
with pkgs;

stdenv.mkDerivation rec {

  name = "whatwhat";

  # Boilerplate for buildable env
  # (nix-build can then be used to create a garbage-collection root)
  # taken from http://datakurre.pandala.org/2015/10/nix-for-python-developers.html
  env = buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';

  buildInputs =
    (with ocaml-ng.ocamlPackages_4_05; [
      ocaml
      camlp4
    ]) ++ 
    [
      # this is for conf-gmp
      gmp
      # this is for the ocaml compiler
      ncurses
      opam
      # this is for the ssl library
      openssl
      # this is for cryptokit
      zlib
    ];

  # shellHook = ''
  #   export PATH=`pwd`/bin:$PATH
  #   if [ ! -e config/coq_config.ml ]; then ./configure -local -annotate -native-compiler no; fi
  # '';
}
