{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable-small";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nur.url = "github:nix-community/NUR";
    home-manager = { 
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    myDrvs.url = "github:huuff/derivations";
    secrets.url = "git+ssh://git@github.com/huuff/secrets.git";

    nix-soapui.url = "github:huuff/nix-soapui";
    nix-portable-shell.url = "github:huuff/nix-portable-shell";
    hm-kubernetes.url = "github:huuff/hm-kubernetes";
  };

  outputs = inputs@{ self, nixpkgs, nix-soapui, nixos-hardware, home-manager, nur, emacs-overlay, myDrvs, secrets, nix-portable-shell, hm-kubernetes }:
  let
    system = "x86_64-linux";
    mkConfig = host: user: extraModules: nixpkgs.lib.nixosSystem rec {
      inherit system;


      specialArgs = { 
        inherit user emacs-overlay nur secrets; 
        myOverlays = myDrvs.overlays;
        myModules = myDrvs.nixosModules;
        # TODO: Maybe it should be in an overlay?
        derivations = {
          soapui57 = nix-soapui.packages.x86_64-linux.default;
        };
        modules = {
          shell = nix-portable-shell.nixosModules.shell;
          kubernetes = hm-kubernetes.nixosModules.kubernetes;
        };
      };

      modules = [
        host

        {
          # Set the registry nixpkgs to the one currently in use
          # This will avoid redownloading nixpkgs on every nix operation
          nix.registry.nixpkgs.flake = nixpkgs; 

          # The netrc file can contain credentials for sources from which to download
          # nix derivations
          nix.extraOptions = ''
            netrc-file = /etc/nix/netrc
          '';
        }

        ./nixos/user.nix
        ./nixos/fonts.nix
        ./nixos/xorg.nix
        ./nixos/audio.nix
        ./nixos/java.nix
        ./nixos/nixconf.nix
        ./nixos/ssh.nix
        ./nixos/virtualization.nix

        home-manager.nixosModules.home-manager
        {
          home-manager.useUserPackages = true;
          home-manager.users.${user} = import ./home/home.nix;
          home-manager.extraSpecialArgs = specialArgs;
        }

        {
          boot.supportedFilesystems = [ "ntfs" ];
        }

        {
          # Use all features of declarative containers for imperative containers
          # https://github.com/erikarvstedt/extra-container
          programs.extra-container.enable = true;
        }
      ] ++ extraModules;
    };
  in
  {

    nixosConfigurations.desktop = mkConfig ./nixos/hosts/desktop/configuration.nix "haf" [];

    nixosConfigurations.office = mkConfig ./nixos/hosts/office/configuration.nix "fran" [
      ({...}: {
        # To use speakers/mic for meetings
        hardware.bluetooth.enable = true;
        services.blueman.enable = true;
      })
    ];

    nixosConfigurations.t420 = mkConfig ./nixos/hosts/t420/configuration.nix "haf"
      [
        ./nixos/wireless.nix { haf.networking.interface = "wlp3s0"; }
        nixos-hardware.nixosModules.lenovo-thinkpad-t420
      ];

  };
}

