{ ... }:
{
  programs.thefuck = {
    enable = true;

    fucks = [
      ./nix_command_not_found.py
    ];
  };
}

