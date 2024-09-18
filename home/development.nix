{ pkgs, ... }:
{
  # TODO maybe these should be tied to the specific editors/repos?
  home.packages = with pkgs; [
    nodePackages.diagnostic-languageserver
    jetbrains.idea-community

    # Bash
    shellcheck
    shfmt
    nodePackages.bash-language-server
  ];
}
