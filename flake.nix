{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    nur.url = "github:nix-community/NUR";
    home-manager = { 
      url = "github:nix-community/home-manager/release-24.11";
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
    hm-kubernetes.url = "github:huuff/hm-kubernetes";

    nix-index-database.url = "github:Mic92/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, nix-soapui, home-manager, nur, emacs-overlay, myDrvs, secrets, nix-portable-shell, hm-kubernetes, scripts, nix-index-database}:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    mkConfig = host: user: extraModules: nixpkgs.lib.nixosSystem rec {
      inherit system;

      specialArgs = { 
        inherit user emacs-overlay nur secrets; 
        scripts = scripts.packages.x86_64-linux;
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

        # For using comma (,) TODO: Maybe put it somewhere else?
        # TODO: The official example (https://github.com/nix-community/nix-index-database) puts nix-index in home-manager
        nix-index-database.nixosModules.nix-index
        {
          programs.nix-index-database.comma.enable = true; 
          # XXX: Can't build it without it
          programs.command-not-found.enable = false;
        }

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
        ./nixos/gnupg.nix
        ./nixos/scanner.nix

        {
          # XXX: I have it enabled in home-manager but that seems to not be enough
          # because of an assertion somewhere
          programs.zsh.enable = true;
        }

        # {
        #   # TODO honestly I need something better than just putting all of my host configs here
        #   services.udev.enable = true;
        #   imports = [ ./home/screens.nix ];
        # }

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

