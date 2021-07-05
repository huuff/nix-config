{ config, pkgs, ... }:

{
  imports =
    [ 
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "office";
  nixpkgs.config.allowUnfree = true;

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  time.timeZone = "Europe/Madrid";

  networking.useDHCP = false;
  networking.interfaces.enp3s0.useDHCP = true;

  console = {
    font = "Lat2-Terminus16";
    keyMap = "es";
  };

  services.xserver.displayManager.setupCommands = ''
    RIGHT='VGA-1'
    LEFT='HDMI-1'
    ${pkgs.xorg.xrandr}/bin/xrandr --output $LEFT --left-of $RIGHT
  '';

  security.wrappers."mount.nfs".source = "${pkgs.nfs-utils.out}/bin/mount.nfs";

  fileSystems."/export/clickferry" = {
    device="/home/fran/clickferry";
    options=["bind"];
  };

  services.nfs.server = {
    enable = true;
    exports = ''
    /export         192.168.56.108(rw,fsid=0,no_subtree_check)
    /export/clickferry  192.168.56.108(rw,nohide,insecure,no_subtree_check)
      '';
  };


  environment.systemPackages = with pkgs; 
  let
    php = pkgs.php74.buildEnv {
      extraConfig = ''
      zend_extension=${pkgs.php74Extensions.xdebug}/lib/php/extensions/xdebug.so
      [xdebug]
      xdebug.mode=debug
      xdebug.client_host=localhost
      xdebug.client_port=9003
      xdebug.start_with_request=yes
        '';
    };
  in 
  [
    php
    php74Extensions.xdebug
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

