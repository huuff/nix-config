{ lib, config, ... }:
{

  programs.rofi = {
    enable = true;

    extraConfig = {
      show-icons = true;
    };

    # Rounded theme adapted from Newman Sanchez (https://github.com/newmanls)
    # Original: https://github.com/newmanls/rofi-themes-collection
    # Modified to use stylix colors
    theme =
      let
        inherit (config.lib.formats.rasi) mkLiteral;
      in lib.mkAfter {
        "*" = {
          margin = mkLiteral "0px";
          padding = mkLiteral "0px";
          spacing = mkLiteral "0px";
        };

        window = {
          location = mkLiteral "north";
          y-offset = mkLiteral "calc(50% - 176px)";
          width = mkLiteral "480";
          border-radius = mkLiteral "24px";
        };

        mainbox = {
          padding = mkLiteral "12px";
        };

        inputbar = {
          border = mkLiteral "2px";
          border-radius = mkLiteral "16px";
          padding = mkLiteral "8px 16px";
          spacing = mkLiteral "8px";
          children = map mkLiteral [ "prompt" "entry" ];
        };

        entry = {
          placeholder = "Search";
        };

        message = {
          margin = mkLiteral "12px 0 0";
          border-radius = mkLiteral "16px";
        };

        textbox = {
          padding = mkLiteral "8px 24px";
        };

        listview = {
          margin = mkLiteral "12px 0 0";
          lines = 8;
          columns = 1;
          fixed-height = false;
        };

        element = {
          padding = mkLiteral "8px 16px";
          spacing = mkLiteral "8px";
          border-radius = mkLiteral "16px";
        };

        element-icon = {
          size = mkLiteral "1em";
          vertical-align = mkLiteral "0.5";
        };
      };
  };
}
