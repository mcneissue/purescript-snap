let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs-unstable {};
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    purescript
    nodejs
    nodePackages.purescript-language-server
    spago
    fish
  ];
}
