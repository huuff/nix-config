{ user, ... }:
{
    services.pulseaudio.enable = false;
    services.pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };

      pulse.enable = true;

      # my mic doesn't work in some computers without this
      # (especially without jack, I think?)
      jack.enable = true;
      wireplumber.enable = true;
    };

    users.users.${user} = {
      extraGroups = [ "audio" ];
    };
}
