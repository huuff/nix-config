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
      allowedTCPPorts = [ 3000 ];
    };
  };
  

  console = {
    font = "Lat2-Terminus16";
    keyMap = "es";
  };
}

