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
      widgets = ["title" "dnd" "notifications" "volume" "backlight"];
      widget-config = {
        volume = {
          label = "";
        };

        backlight = {
          label = "";
          # TODO: don't hardcode?
          device = "amdgpu_bl1";
        };
      };
    };
  };

}