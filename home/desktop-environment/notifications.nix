{ lib, config, ... }:
{

  services.swaync = {
    enable = true;
    # CONTRIB: Doesn't stylix set opacity? maybe I could write a PR
    style = lib.mkAfter ''
      .notification {
        opacity: ${toString config.stylix.opacity.popups};
      }

      .control-center {
        opacity: ${toString config.stylix.opacity.popups};
      }
    '';
    settings = {
      positionX = "left";
    };
  };

}