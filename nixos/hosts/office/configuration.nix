{ config, pkgs, ... }:

{
  imports =
    [ 
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "office";

  time.timeZone = "Europe/Madrid";

  networking = {
    useDHCP = false;
    interfaces.enp3s0.useDHCP = true;
    firewall.allowedTCPPorts = [
      9003 # For PHP's xdebug
    ];
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "es";
  };

  services.xserver.displayManager.setupCommands = ''
    RIGHT='VGA-1'
    LEFT='HDMI-1'
    ${pkgs.xorg.xrandr}/bin/xrandr --output $LEFT --left-of $RIGHT
  '';


  # TODO: Unmantained... do I need it? There's also nix-phps
  #environment.systemPackages = with pkgs; 
  #let
  #  php = pkgs.php74.buildEnv {
  #    extraConfig = ''
  #    zend_extension=${pkgs.php74Extensions.xdebug}/lib/php/extensions/xdebug.so
  #    [xdebug]
  #    xdebug.mode=debug
  #    xdebug.client_host=localhost
  #    xdebug.client_port=9003
  #    xdebug.start_with_request=yes
  #      '';
  #  };
  #in 
  #[
  #  php
  #  php74Extensions.xdebug
  #];


  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

