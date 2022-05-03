{ pkgs, ... }:
{
  home.packages = with pkgs; [
    nodePackages.diagnostic-languageserver
    jetbrains.idea-ultimate

    # Nix
    nixpkgs-fmt
    rnix-lsp

    # Bash
    shellcheck
    shfmt
    nodePackages.bash-language-server
  ];
}
