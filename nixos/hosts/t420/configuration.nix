# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports =
    [
    ./hardware-configuration.nix
  ];
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  boot.kernelParams = [ "intel_pstate=active" ];

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  nixpkgs.config.allowUnfree = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "t420";

  time.timeZone = "Europe/Madrid";

  networking.useDHCP = false;
  networking.interfaces.enp0s25.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;

  console = {
    font = "Lat2-Terminus16";
    keyMap = "es";
  };

  users.users.haf = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };


    environment.systemPackages = with pkgs; [
    jdk16_headless
  ];

  environment.etc = with pkgs; {
    "jdk16".source = jdk16_headless;
  };
}

