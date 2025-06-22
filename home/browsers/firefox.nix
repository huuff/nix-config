{pkgs, nur, ...}:
let 
  firefox-addons = pkgs.nur.repos.rycee.firefox-addons;
in {
  nixpkgs.overlays = [ nur.overlay ];

  programs.firefox = {
    enable = true;
    profiles = {
      default = {
        id = 0;
        isDefault = true;
        search = {  
          default = "DuckDuckGo";
          force = true;
        };

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
          libredirect
        ];
      };

      workgpt = {
        id = 1;
        isDefault = false;
        search = {
          default = "DuckDuckGo";
          force = true;
        };
      };
    };
  };

}
