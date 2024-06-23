{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell rec {
  SHELL_NAME = "OCAML";
  nativeBuildInputs = with pkgs.ocamlPackages; [ ocaml findlib ocamlformat dune_3 ocaml-lsp utop ocamlformat-rpc-lib ];
  buildInputs = with pkgs.ocamlPackages; [ angstrom yojson];

  # build tools
  # dependencies
  shellHook = ''
    export OCAML_LSP_SERVER=$(which ocamllsp)
  '';
}

