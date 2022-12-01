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

    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      ublock-origin
      leechblock-ng 
      i-dont-care-about-cookies
      decentraleyes
      vue-js-devtools
      languagetool
      ublacklist
      privacy-badger
      https-everywhere
      react-devtools
      wallabagger
      darkreader
      onepassword-password-manager
    ];
  };
}
