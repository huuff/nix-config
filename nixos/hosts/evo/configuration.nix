{ config, pkgs, lib, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ../../flatpak.nix
    ./disko.nix
  ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    initrd = {
      availableKernelModules = [ "nvme" "xhci_pci" "thunderbolt" "usb_storage" "sd_mod" "sdhci_pci" ];
      kernelModules = [ "kvm-amd" ];
    };

    extraModulePackages = [];
  };

  nixpkgs.hostPlatform = "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  networking.hostName = "evo";

  time.timeZone = "Europe/Madrid";

  networking = {
    useDHCP = false;
    firewall = {
      enable = true;
      allowedTCPPorts = [
        3000 # next.js applications
      ];
    };
    interfaces = {
      wlp99s0.useDHCP = true;
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  swapDevices = [
    {
      device = "/.swapfile";
      size = 16 * 1024; # 16 GB
    }
  ];

  system.activationScripts.setupRootSwapfile = {
    text = ''
      if [ ! -e /.swapfile ]; then
        touch /.swapfile
        # remove CoW, necessary for swap on btrfs
        ${pkgs.e2fsprogs}/bin/chattr +C /.swapfile
        fallocate -l 16G /.swapfile 
        chmod 600 /.swapfile
        mkswap /.swapfile
      fi
    '';
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?
}

