{
  config,
  pkgs,
  lib,
  modulesPath,
  ...
}:

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
      availableKernelModules = [
        "nvme"
        "xhci_pci"
        "thunderbolt"
        "usb_storage"
        "sd_mod"
        "sdhci_pci"
      ];
      kernelModules = [ "kvm-amd" ];
    };

    kernelPackages = pkgs.linuxPackages_latest;

    # this hides some ugly ACPI errors during boot that garble the login screen
    # and are totally useless
    consoleLogLevel = 3;

    resumeDevice = "/dev/mapper/cryptroot";

    kernelParams = [
      # suppossedly can fix issues with everything that goes through a docking station disconnecting, but I'm not
      # seeing any good results. I'm leaving it just in case there's any chance it helps
      "usbcore.autosuspend=-1"
      # necessary for hibernate, run
      # `sudo btrfs inspect-internal map-swapfile -r /.swapfile`
      "resume_offset=74196224"
    ];

    extraModulePackages = [
      # needed for ethernet I guess?
      config.boot.kernelPackages.yt6801
    ];
  };

  # apparently this makes many dynamically-linked executables *just work*
  # without nixos wizardry
  programs.nix-ld.enable = true;

  services.logind.lidSwitch = "hibernate";

  # todo put this somewhere else
  hardware.ledger.enable = true;

  # mostly just use it for sending reminders to myself
  services.atd.enable = true;

  programs.firejail.enable = true;
  # XXX laptop keyboard and touchpad stop working after a suspend without this
  powerManagement.resumeCommands = "${pkgs.kmod}/bin/rmmod atkbd; ${pkgs.kmod}/bin/modprobe atkbd reset=1";

  nixpkgs.hostPlatform = "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  networking.hostName = "evo";

  time.timeZone = "Europe/Madrid";

  networking = {
    useDHCP = true; # needed for wired Ethernet (WiFi DHCP is handled by iwd independently)
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

  swapDevices = [
    {
      device = "/.swapfile";
      size = 34 * 1024; # 34 GB — must be >= RAM (32 GB) for hibernation
    }
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?
}
