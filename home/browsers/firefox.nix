{pkgs, nur, ...}:
let 
  firefox-addons = pkgs.nur.repos.rycee.firefox-addons;
in {
  nixpkgs.overlays = [ nur.overlays.default ];

  programs.firefox = {
    enable = true;
    profiles = {
      default = {
        id = 0;
        isDefault = true;
        search = {  
          default = "ddg";
          force = true;
        };

        extensions.packages = with firefox-addons; [
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
          libredirect
          youtube-no-translation
        ];
      };

      workgpt = {
        id = 1;
        isDefault = false;
        search = {
          default = "ddg";
          force = true;
        };
      };
    };
  };

}
