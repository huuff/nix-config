{ config, lib, secrets, ... }:
  {

      networking = {
        wireless = {
          enable = true;  # Enables wireless support via wpa_supplicant.
          userControlled.enable = true;
          interfaces = [ "wlp3s0" ]; # TODO: make this configurable, without it wireless fails to start sometimes
          networks = secrets.networks;
        };
      };
  }
