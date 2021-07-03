{

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nur.url = "github:nix-community/NUR";
    home-manager.url = "github:nix-community/home-manager";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    secrets.url = "git+ssh://git@github.com/huuff/secrets.git";
  };

  outputs = { self, nixpkgs, nixos-hardware, home-manager, nur, emacs-overlay, secrets }:
  let
    pkgs = import nixpkgs {
      system = "x86_64-linux";
    };
  in
  {
    nixosConfigurations.t420 = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ 
        { nixpkgs.overlays = [ nur.overlay emacs-overlay.overlay ];}
        ./nixos/hosts/t420/configuration.nix
        ./nixos/wireless.nix { wifi.networks = secrets.networks; }
        ./nixos/fonts.nix
        ./nixos/xorg.nix
        ./nixos/cachix.nix
        ./nixos/audio.nix { users.audio = "haf"; }
        nixos-hardware.nixosModules.lenovo-thinkpad-t420

        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.haf = (import ./home/home.nix) secrets;
        }
      ];
    };
    nixosConfigurations.desktop =
      nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
        { nixpkgs.overlays = [ nur.overlay emacs-overlay.overlay ];}
          ./nixos/hosts/desktop/configuration.nix
          ./nixos/fonts.nix
          ./nixos/xorg.nix
          ./nixos/cachix.nix
          ./nixos/audio.nix { users.audio = "haf"; }

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.haf = (import ./home/home.nix) secrets;
          }

        ];
      };
      nixosConfigurations.office = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
        { nixpkgs.overlays = [ nur.overlay emacs-overlay.overlay ];}
          ./nixos/hosts/office/configuration.nix
          ./nixos/fonts.nix
          ./nixos/xorg.nix
          ./nixos/audio.nix { users.audio = "fran"; }

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.fran = (import ./home/home.nix) secrets;
          }

        ];
      };
    };
  }

