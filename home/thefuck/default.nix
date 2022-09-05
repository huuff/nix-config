{ myModules, ... }:
{
  imports = [ myModules.thefuck ];

  programs.thefuck = {
    enable = true;

    enableFishIntegration = false;

    fucks = [
      ./nix_command_not_found.py
    ];
  };
}

