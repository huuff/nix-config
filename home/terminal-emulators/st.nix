{pkgs, ...}:

{
  programs.st = {
    enable = true;
    colorscheme = "dracula";
    fontSize = 16;
    scrollback = true;
    blinkingCursor = true;
    exec = "tmux";
    flags = [
      {f = "FiraCode Nerd Font";}
    ];
  };

}
