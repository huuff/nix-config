{ pkgs, ... }:

{
  imports =
    [ 
      ./hardware-configuration.nix
    ];


    boot = {
      loader.systemd-boot.enable = true;
      loader.efi.canTouchEfiVariables = true;
      kernelParams = [ 
        "i915.force_probe=46a6"
        "ibt=off" # otherwise VirtualBox breaks?
      ]; 
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
        3000 # For live-viewing frontend projects
        5173 # For live-viewing vite projects
        8080 # Some other web apps
      ];
    };

    extraHosts = ''
      127.0.0.1 office.local
    '';
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "es";
  };


  services = {
    printing.enable = true;
    avahi = {
      enable = true;
      openFirewall = true;
      #nssmdns = true;
      publish = {
        enable = true;
        addresses = true;
        domain = true;
        hinfo = true;
        userServices = true;
        workstation = true;
      };
    };
    xserver.displayManager.setupCommands = ''
        LEFT='DP-1'
        RIGHT='HDMI-1'
        ${pkgs.xorg.xrandr}/bin/xrandr --output "$LEFT" --left-of "$RIGHT"
    '';

    # TODO: I need this shit for postman since they broke nixpkgs, maybe I should put this for all?
    flatpak.enable = true;
  };

  # this shit is necessary for flatpack
  xdg.portal = {
    enable = true;  
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.common.default = "gtk";
  };

  # XXX: Fix nssmdns issues (https://discourse.nixos.org/t/help-with-local-dns-resolution/20305/6?u=haf)
  system.nssModules = pkgs.lib.optional true pkgs.nssmdns;
  system.nssDatabases.hosts = pkgs.lib.optionals true (pkgs.lib.mkMerge [
    (pkgs.lib.mkBefore [ "mdns4_minimal [NOTFOUND=return]" ]) # before resolve
    (pkgs.lib.mkAfter [ "mdns4" ]) # after dns
  ]);


  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

