# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports =
    [
    ./hardware-configuration.nix
  ];
  # It gets too slow without this
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  boot.kernelParams = [ "intel_pstate=active" ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "zen";

  time.timeZone = "Europe/Madrid";

  networking = {
    useDHCP = false;
    interfaces = {
      wlp1s0.useDHCP = true;
    };

    firewall = {
      enable = false; # TODO: Just enable ports for DLNA (but it doesn't work with my files either)
      #allowedTCPPorts = [ 3000 ];
    };
  };
  

  console = {
    font = "Lat2-Terminus16";
    keyMap = "es";
  };

  # TODO: Put it in a separate module so I can reuse it?
  services.minidlna = {
    enable = true;
    settings = {
      friendly_name = "DLNA MEDIA";
      media_dir = [
        "V,/mnt/media/films/" #Videos files are located here
        #"A,/mnt/media/songs/" #Audio files are here
      ];
      log_level = "error";
      inotify = "yes"; # Needs inotify-tools... do I have it?
    };
  };
  users.users.minidlna = {
    extraGroups = [ "users" ]; # so minidlna can access the files.
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}

