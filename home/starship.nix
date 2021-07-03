{ pkgs, ... }:
{
  programs.starship = {
    enable = true;
    settings = {
      add_newline = false;
      custom.gitname = {
        command = "git config user.email";
        when = "git rev-parse --is-inside-work-tree";
      };
    };
  };
}
