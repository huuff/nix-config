{ pkgs, ... }:
{
  # TODO maybe these should be tied to the specific editors/repos?
  home.packages = with pkgs; [
    diagnostic-languageserver
    jetbrains.idea-oss

    # Bash
    shellcheck
    shfmt
    bash-language-server
  ];
}
