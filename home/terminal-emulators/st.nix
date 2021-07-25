{ pkgs, myOverlays, myModules, ...}:

{
  imports = [ myModules.st ];

  nixpkgs.overlays = [ myOverlays.st ];

  programs.st = {
    enable = true;
    colorscheme = "dracula";
    fontSize = 16;
    scrollback = false; # I use tmux for it
    blinkingCursor = true;
    exec = "tmux";
    flags = [
      {f = "FiraCode Nerd Font";}
    ];
  };

}
