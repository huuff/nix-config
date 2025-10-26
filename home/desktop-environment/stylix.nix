{ pkgs, ... }:
{
  stylix = {
    enable = true;
    polarity = "dark";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/tomorrow-night.yaml";
    icons = {
      enable = true;
      package = pkgs.gruvbox-plus-icons;
      light = "Gruvbox-Plus-Light";
      dark = "Gruvbox-Plus-Dark";
    };
    fonts = {
      monospace.package = pkgs.nerd-fonts.fira-code;
      # serif.package = pkgs.nerd-fonts.fira-code;
      # sansSerif.package = pkgs.nerd-fonts.fira-code;

      sizes = {
        terminal = 10;
      };
    };

    targets.firefox.profileNames = ["default"];
  };
}
