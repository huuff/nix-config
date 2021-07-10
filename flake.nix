{

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nur.url = "github:nix-community/NUR";
    home-manager.url = "github:nix-community/home-manager";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    mydrvs.url = "github:huuff/derivations";
    secrets.url = "git+ssh://git@github.com/huuff/secrets.git";
  };

  outputs = { self, nixpkgs, nixos-hardware, home-manager, nur, emacs-overlay, mydrvs, secrets }:
  let
    mkConfig = host: mainUser: extraModules : nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        host

        { 
          nixpkgs.overlays = [ 
          nur.overlay 
          emacs-overlay.overlay 
          mydrvs.overlays.tmux-plugins 
          mydrvs.overlays.st
        ];
      }
        ./nixos/user.nix { users.mainUser = mainUser; }
        ./nixos/fonts.nix
        ./nixos/xorg.nix
        ./nixos/cachix.nix
        ./nixos/audio.nix

        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.${mainUser} = (import ./home/home.nix) secrets mainUser mydrvs;
        }
      ] ++ extraModules;
    };
  in
  {
    nixosConfigurations.t420 = mkConfig ./nixos/hosts/t420/configuration.nix "haf"
    [
      ./nixos/wireless.nix { wifi.networks = secrets.networks; }
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

