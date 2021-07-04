# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
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

  users.users.fran = {
    extraGroups = [ "docker" ]; #TODO: Put docker into a separate file
  };

  services.xserver.displayManager.setupCommands = ''
    RIGHT='VGA-1'
    LEFT='HDMI-1'
    ${pkgs.xorg.xrandr}/bin/xrandr --output $LEFT --left-of $RIGHT
  '';

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "fran" ];

  virtualisation.libvirtd.enable = true;
  users.extraUsers.fran.extraGroups = [ "libvirtd" ];
  networking.firewall.checkReversePath = false;

  virtualisation.docker.enable = true;


  networking.firewall.extraCommands = ''
    ip46tables -I INPUT 1 -i vboxnet+ -p tcp -m tcp --dport 2049 -j ACCEPT
  '';
  networking.firewall.allowedTCPPorts = [ 2049 ];

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
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

