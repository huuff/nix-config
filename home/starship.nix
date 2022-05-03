{ pkgs, ... }:
{
  programs.starship = {
    enable = true;
    settings = {
      add_newline = false;
      # Show my email on prompt so I can know whether I'm in a work repo or personal repo (and also whether its any of these, but misconfigured)
      custom.gitname = {
        command = "git config user.email";
        when = "git rev-parse --is-inside-work-tree";
      };
    };
  };
}
