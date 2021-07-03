{ pkgs, ... }:
{
  fonts = {
    fonts = with pkgs; [
      fira-code
      fira-code-symbols
      (nerdfonts.override { fonts = [ "FiraCode" ]; })
    ];
    fontconfig.antialias = true;
  };
}
