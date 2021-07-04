{ config, lib, ... }:
with lib;
{
  config = {
    # I'm not sure whether this works, I have like 3 different ways of setting groups
    # in my previous files. Check it out.
    users.users.${config.users.mainUser}.extraGroups = [ "docker" "libvirtd" "vboxusers" ];

    virtualisation = {
      virtualbox.host.enable = true;
      docker.enable = true;
      libvirtd.enable = true;
    };

    # Not entirely sure why I need this, might even be dangerous
    networking.firewall.extraCommands = ''
      ip46tables -I INPUT 1 -i vboxnet+ -p tcp -m tcp --dport 2049 -j ACCEPT
    '';
    networking.firewall.allowedTCPPorts = [ 2049 ];
    networking.firewall.checkReversePath = false;
  };

}
