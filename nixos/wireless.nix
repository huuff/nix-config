{ config, lib, networks, ... }:
with lib;
let
  cfg = config.wifi;
in
  {
    options.wifi.networks = mkOption {
      type = types.attrs;
      default = {};
      description = "Networks with their PSKs";
    };

    config = {
      networking = {
        wireless = {
          enable = true;  # Enables wireless support via wpa_supplicant.
          userControlled.enable = true;
          interfaces = [ "wlp3s0" ]; # TODO: make this configurable
          networks = cfg.networks;
        };
      };
    };
  }
