{pkgs, nur, ...}:
{
  nixpkgs.overlays = [ nur.overlay ];

  programs.firefox = {
    enable = true;
    profiles = {
      default = {
        isDefault = true;
      };
    };

    # TODO: Seems like this isn't getting updated? I'm missing:
    # * wallabag
    # * vue
    # others I have at work but not here (installed manually)
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      ublock-origin
      leechblock-ng
      i-dont-care-about-cookies
      decentraleyes
      privacy-badger
      https-everywhere
      pkgs.nur.repos.rycee.firefox-addons."1password-x-password-manager"
    ];
  };
}
