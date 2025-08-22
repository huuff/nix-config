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
        ] ++ [
          # I use it for CSS testing
          (buildFirefoxXpiAddon {
            pname = "toggle-dark-mode";
            version = "1.0.1";
            addonId = "{6bd40d01-481e-451f-9582-22bde8083fe3}";
            url = "https://addons.mozilla.org/en-US/firefox/addon/toggle-dark-mode/";
            sha256 = "sha256-efrD7qPk7qPfJ02Rm+hKOuP11QEad4tcZcgmLh67D2M=";
            meta = {
              homepage = "https://addons.mozilla.org/en-US/firefox/addon/toggle-dark-mode/";
              description = "Toggle the content’s color scheme between preferring light or dark (or inheriting)";
              license = pkgs.lib.licenses.wtfpl;
            };
          })
        ];
      };
    };
  };

}
