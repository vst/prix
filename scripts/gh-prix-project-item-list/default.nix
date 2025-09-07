## Test:
##
## nix-build --expr 'let pkgs = import <nixpkgs> {}; in pkgs.callPackage ./default.nix {}'

{ writeShellApplication
, stdenv
  ## Runtime dependencies:
, bash
, gh
, jq
, ...
}:

let
  name = "gh-prix-project-item-list";
  data = stdenv.mkDerivation {
    name = "${name}-data";
    src = ./.;
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out/share/
      cp $src/*.{gql,jq} $out/share/
    '';
  };
in
writeShellApplication {
  inherit name;
  text = builtins.readFile ./script.sh;
  runtimeInputs = [ data bash gh jq ];
  runtimeEnv = {
    GH_PRIX_PROJECT_ITEM_LIST = "${data}/share";
  };
}
