let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs-unstable {};
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    purescript
    spago
    nodejs
    nodePackages.purescript-language-server
    fish
  ];
}
