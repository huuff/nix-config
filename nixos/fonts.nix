{ pkgs, ... }:
{
  fonts = {
    packages = with pkgs; [
      fira-code
      fira-code-symbols
      hack-font
      nerd-fonts.fira-code
      nerd-fonts.hack
    ];
    fontconfig.antialias = true;
  };
}
