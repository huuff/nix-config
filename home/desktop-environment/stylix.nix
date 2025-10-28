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
      # XXX I'd say setting the names is necessary, for example firefox seems not to pick the font if the name isn't set
      monospace = {
        package = pkgs.nerd-fonts.fira-code;
        name = "FiraCode Nerd Font";
      } ;

      # TODO: definitely don't use the monospaced font everywhere
      # serif = {
      #   package = pkgs.nerd-fonts.fira-code;
      #   name = "FiraCode Nerd Font";
      # } ;
      #
      # sansSerif = {
      #   package = pkgs.nerd-fonts.fira-code;
      #   name = "FiraCode Nerd Font";
      # } ;

      sizes = {
        terminal = 10;
      };
    };

    cursor = {
      package = pkgs.phinger-cursors;
      name = "phinger-cursors-dark";
      size = 24;
    };

    opacity = {
      popups = 0.75;
    };

    targets.firefox.profileNames = ["default"];
  };
}
