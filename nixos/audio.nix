{ config, lib, ... }:
with lib;
{
  config = {
    sound.enable = true;
    hardware.pulseaudio.enable = true;
    hardware.pulseaudio.support32Bit = true;

    users.users.${config.users.mainUser} = {
      extraGroups = [ "audio" ];
    };
  };
}
