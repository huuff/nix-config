{
  pkgs,
  lib,
  user,
  ...
}:
with lib;
{
  config = {
    # I'm not sure whether this works, I have like 3 different ways of setting groups
    # in my previous files. Check it out.
    users.users.${user}.extraGroups = [
      "docker"
      "libvirtd"
      "kvm"
    ];

    # Necessary for virt-manager
    programs.dconf.enable = true;

    # Necessary for shared folders with virt-manager
    environment.systemPackages = with pkgs; [
      virtiofsd
    ];

    virtualisation = {
      # TODO: Enable UID remapping!
      # TODO: Actually, remove it when I fix my usages that depend on root (mainly, COPY instructions) and use podman
      docker = {
        enable = true;
        autoPrune.enable = true;
        # Docker's default 172.17.0.0/16 conflicts with my mobile hotspot (172.17.96.0/24)
        daemon.settings = {
          bip = "10.200.0.1/24";
          default-address-pools = [
            {
              base = "10.201.0.0/16";
              size = 24;
            }
          ];
        };
      };
      libvirtd.enable = true;
      podman.enable = true;

    };
  };

}
