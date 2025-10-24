{pkgs, ...}:
{
  gtk = {
    enable = true;
    theme = {
      name = "Arc-Dark";
      package = pkgs.arc-theme;
    };
    iconTheme = {
      name = "Tela";
      package = pkgs.tela-icon-theme;
    };
    cursorTheme = {
      name = "Adwaita"; # A good default, or choose another like Bibata
      package = pkgs.adwaita-icon-theme;
      size = 24;
    };
  };
}
