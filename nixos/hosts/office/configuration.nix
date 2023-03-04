{ config, pkgs, ... }:

{
  imports =
    [ 
    ./hardware-configuration.nix
  ];


  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    kernelParams = [ "i915.force_probe=46a6" ]; 
    # XXX: I had to use the latest kernel because of some driver incompatibility with the Alder Lake CPU
    kernelPackages = pkgs.linuxPackages_latest;
  };

  time.timeZone = "Europe/Madrid";

  networking = {
    hostName = "office";
    useDHCP = false;
    interfaces.enp2s0.useDHCP = true;
    firewall = {
      enable = false;
      allowedTCPPorts = [
        9003 # For PHP's xdebug
      ];
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "es";
  };

  services.printing.enable = true;
  services.avahi = {
    enable = true;
    openFirewall = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

