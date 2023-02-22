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
      };
    };

    extensions = with firefox-addons; [
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
    ] ++ [
      (firefox-addons.buildFirefoxXpiAddon {
        pname = "apollo-client-devtools";
        version = "4.1.1";
        addonId = "{a5260852-8d08-4979-8116-38f1129dfd22}";
        url = "https://addons.mozilla.org/firefox/downloads/file/3958894/apollo_developer_tools-4.1.1.xpi";
        sha256 = "rZNnjSvWxgGqp6B04O4an9lryrBW6M49SdSOn+b+s6A=";
        meta = with pkgs.lib;
        {
          homepage = "https://addons.mozilla.org/es/firefox/addon/apollo-developer-tools/";
          description = "DevTools for the GraphQL Apollo Client";
          license = licenses.mit;
          platforms = platforms.all;
        };
      })
      (firefox-addons.buildFirefoxXpiAddon {
        pname = "keepa";
        version = "4.10";
        addonId = "amptra@keepa.com";
        url = "https://addons.mozilla.org/firefox/downloads/file/4041807/keepa-4.10.xpi";
        sha256 = "RzoedFBl0FTlkAmaHLgib8Rm2ePtpZYnEbzvvPOOeyQ=";
        meta = with pkgs.lib;
        {
          homepage = "https://addons.mozilla.org/es/firefox/addon/keepa/";
          description = "Keepa.com - Amazon Price Tracker";
          license = licenses.mit;
          platforms = platforms.all;
        };
      })
    ];
  };
}
