{ config, lib, user, ... }:
{
    #sound.enable = true;
    #hardware.pulseaudio.enable = true;
    #hardware.pulseaudio.support32Bit = true;
    services.pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };

      pulse.enable = true;
    };

    users.users.${user} = {
      extraGroups = [ "audio" ];
    };
}
