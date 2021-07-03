{ config, lib, ... }:

with lib;
let
  cfg = config.users;
in {

  options.users.audio = mkOption {
    type = with types; str;
    default = null;
    description = "User to add to the audio group";
  };

  config = {
    sound.enable = true;
    hardware.pulseaudio.enable = true;
    hardware.pulseaudio.support32Bit = true;

    users.users.${cfg.audio} = {
      extraGroups = [ "audio" ];
    };
  };
}
