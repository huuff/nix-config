{ pkgs, ... }:
{
  fonts = {
    packages = with pkgs; [
      fira-code
      fira-code-symbols
      hack-font
      (nerdfonts.override { fonts = [ "FiraCode" "Hack"]; })
    ];
    fontconfig.antialias = true;
  };
}
