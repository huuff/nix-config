{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    nur.url = "github:nix-community/NUR";
    home-manager = { 
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    myDrvs.url = "github:huuff/derivations";
    secrets.url = "git+ssh://git@github.com/huuff/secrets.git";
    scripts = {
      url = "github:huuff/nix-scripts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-soapui.url = "github:huuff/nix-soapui";
    nix-portable-shell.url = "github:huuff/nix-portable-shell";
    nix-home-modules = {
      url = "github:huuff/nix-home-modules";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hm-kubernetes.url = "github:huuff/hm-kubernetes";

    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-unstable,
    nix-soapui,
    home-manager,
    nix-home-modules,
    nur,
    emacs-overlay,
    myDrvs,
    secrets,
    nix-portable-shell,
    hm-kubernetes,
    scripts,
    nix-index-database,
    sops-nix
  }:
  let
    system = "x86_64-linux";

    pkgs = import nixpkgs { inherit system; };

    unstablePkgs = import nixpkgs-unstable { inherit system; };

    mkConfig = host: user: extraModules: nixpkgs.lib.nixosSystem rec {
      inherit system;

      specialArgs = { 
        inherit user emacs-overlay nur secrets unstablePkgs; 

        scripts = scripts.packages.x86_64-linux;
        myModules = myDrvs.nixosModules;
        # TODO: Maybe it should be in an overlay?
        derivations = {
          soapui57 = nix-soapui.packages.x86_64-linux.default;
        };
        modules = {
          shell = nix-portable-shell.nixosModules.shell;
          kubernetes = hm-kubernetes.nixosModules.kubernetes;
        };

        homeModules = pkgs.lib.fold (s1: s2: s1 // s2) {} [
          nix-home-modules.homeManagerModules
          sops-nix.homeManagerModules
        ];

      };

      modules = [
        host

        # For using comma (,) TODO: Maybe put it somewhere else?
        # TODO: The official example (https://github.com/nix-community/nix-index-database ) puts nix-index in home-manager
        nix-index-database.nixosModules.nix-index
        {
          programs.nix-index-database.comma.enable = true; 
          # XXX: Can't build it without it
          programs.command-not-found.enable = false;
        }

        {
          # Set the registry nixpkgs to the one currently in use
          # This means that when you do `nix run nixpkgs#something` you're running from the same
          # nixpkgs as you've set in the system-wide config
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
        ./nixos/gnupg.nix
        ./nixos/scanner.nix

        {
          # XXX: I have it enabled in home-manager but that seems to not be enough
          # because of an assertion somewhere
          programs.zsh.enable = true;
        }

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

    nixosConfigurations = {
      zen = mkConfig ./nixos/hosts/zen/configuration.nix "haf"
      [
        ./nixos/wireless.nix { haf.networking.interface = "wlp1s0"; }
        ./nixos/bluetooth.nix
        ./nixos/battery.nix
      ];
    };

    # TODO formatting and pre-commit
    devShells.${system}.default = with pkgs; mkShell {
        buildInputs = [
          nil
          git-crypt
        ];
    };
  };
}

