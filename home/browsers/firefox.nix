{pkgs, nur, ...}:
let 
  firefox-addons = pkgs.nur.repos.rycee.firefox-addons;
in {
  nixpkgs.overlays = [ nur.overlay ];

  programs.firefox = {
    enable = true;
    profiles = {
      default = {
        isDefault = true;

        extensions = with firefox-addons; [
          ublock-origin
          leechblock-ng 
          decentraleyes
          languagetool
          privacy-badger
          react-devtools
          wallabagger
          darkreader
          onepassword-password-manager
          bitwarden
          istilldontcareaboutcookies
        ];
      };
    };
  };

}
