{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.useOSProber = true;
  networking.hostName = "desktop";
 
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Set your time zone.
  time.timeZone = "Europe/Madrid";

  networking.useDHCP = false;
  networking.interfaces.enp3s0.useDHCP = true;

  console = {
     font = "Lat2-Terminus16";
     keyMap = "es";
   };

  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs;
  [
    jdk16_headless
  ];

  environment.etc = with pkgs; {
    "jdk16".source = jdk16_headless;
  };
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

