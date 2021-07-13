{ config, lib, user, ... }:
with lib;
{
  config = {
    sound.enable = true;
    hardware.pulseaudio.enable = true;
    hardware.pulseaudio.support32Bit = true;

    users.users.${user} = {
      extraGroups = [ "audio" ];
    };
  };
}
