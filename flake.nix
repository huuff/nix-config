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

  outputs = inputs@{ self, nixpkgs, nixos-hardware, home-manager, nur, emacs-overlay, mydrvs, secrets }:
  let
    system = "x86_64-linux";
    mkConfig = host: user: extraModules : nixpkgs.lib.nixosSystem rec {
      inherit system;

      specialArgs = { 
        inherit inputs user emacs-overlay nur; 
        myOverlays = mydrvs.overlays;
      };

      modules = [
        host

        ./nixos/user.nix
        ./nixos/fonts.nix
        ./nixos/xorg.nix
        ./nixos/cachix.nix
        ./nixos/audio.nix

        home-manager.nixosModules.home-manager
        {
          #home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.${user} = import ./home/home.nix;
          home-manager.extraSpecialArgs = specialArgs;
        }
      ] ++ extraModules;
    };
  in
  {
    nixosConfigurations.t420 = mkConfig ./nixos/hosts/t420/configuration.nix "haf"
    [
      ./nixos/wireless.nix { wifi.networks = secrets.networks; } # TODO: use inputs specialArgs to improve this, but do it on my t420
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

