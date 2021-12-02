{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nur.url = "github:nix-community/NUR";
    home-manager = { 
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    myDrvs.url = "github:huuff/derivations";
    secrets.url = "git+ssh://git@github.com/huuff/secrets.git";
  };

  outputs = inputs@{ self, nixpkgs, nixos-hardware, home-manager, nur, emacs-overlay, myDrvs, secrets }:
  let
    system = "x86_64-linux";
    mkConfig = host: user: extraModules: nixpkgs.lib.nixosSystem rec {
      inherit system;


      specialArgs = { 
        inherit user emacs-overlay nur secrets; 
        myOverlays = myDrvs.overlays;
        myModules = myDrvs.nixosModules;
      };

      modules = [
        host

        {
          # Set the registry nixpkgs to the one currently in use
          # This will avoid redownloading nixpkgs on every nix operation
          nix.registry.nixpkgs.flake = nixpkgs; 
        }

        ./nixos/user.nix
        ./nixos/fonts.nix
        ./nixos/xorg.nix
        ./nixos/cachix.nix
        ./nixos/audio.nix
        ./nixos/java.nix
        ./nixos/nixconf.nix

        home-manager.nixosModules.home-manager
        {
          home-manager.useUserPackages = true;
          home-manager.users.${user} = import ./home/home.nix;
          home-manager.extraSpecialArgs = specialArgs;
        }

        {
          boot.supportedFilesystems = [ "ntfs" ];
        }
      ] ++ extraModules;
    };
  in
  {
    nixosConfigurations.t420 = mkConfig ./nixos/hosts/t420/configuration.nix "haf"
    [
      ./nixos/wireless.nix { haf.networking.interface = "wlp3s0"; }
      nixos-hardware.nixosModules.lenovo-thinkpad-t420
    ];
    nixosConfigurations.desktop = mkConfig ./nixos/hosts/desktop/configuration.nix "haf" 
    [
      ./nixos/virtualization.nix
    ];
    nixosConfigurations.office = mkConfig ./nixos/hosts/office/configuration.nix "fran" 
    [
      ./nixos/virtualization.nix
    ];

  };
}

