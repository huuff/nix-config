{ config, lib, ... }:
with lib;
{
  options = {
    haf.networking.interface = mkOption {
      type = types.str;
      default = null;
      example = "wlp3s0";
    };
  };

  config = {
    assertions = [
      {
        assertion = config.haf.networking.interface != null;
        message = "You have to set a networking interface (haf.networking.interface) or else the wireless will randomly fail to start at boot";
      }
    ];

    networking = {
      wireless = {
        enable = true;  # Enables wireless support via wpa_supplicant.
        #userControlled.enable = true;
        interfaces = [ config.haf.networking.interface ];
      };
    };

    environment.etc."wpa_supplicant.conf" = {
      source = config.sops.secrets.wpaSupplicantConf.path;
      enable = true;
      mode = "0600";
    };
  };
}
