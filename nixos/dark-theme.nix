{ pkgs, ... }:
{
  # NOTE: This has to be done at the system level, rather the home manager because otherwise
  # there are apps that won't pick it up. Some examples I suffered:
  # * chromium
  # * slack
  # * flatpaks

  # environment.systemPackages = with pkgs;[
  #   adw-gtk3
  # ];
  #
  # programs.dconf = {
  #   enable = true;
  #   profiles.user.databases = [{
  #     settings = {
  #       "org/gnome/desktop/interface" = {
  #         color-scheme = "prefer-dark";
  #         gtk-theme = "adw-gtk3-dark";
  #       };
  #     };
  #   }];
  # };
  #
  # qt = {
  #   enable = true;
  #   platformTheme = "gtk2";
  #   style = "adwaita-dark";
  # };

}
