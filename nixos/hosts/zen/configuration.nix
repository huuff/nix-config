{ pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./screens.nix
    ../../flatpak.nix
  ];
  # It gets too slow without this
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [ "intel_pstate=active" ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "zen";

  time.timeZone = "Europe/Madrid";

  networking = {
    useDHCP = false;
    firewall = {
      enable = true;
      allowedTCPPorts = [
        3000 # next.js applications
      ];
    };

  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
