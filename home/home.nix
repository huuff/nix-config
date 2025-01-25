{ pkgs, user, secrets, myModules, derivations, modules, scripts, ... }:
{
    imports = [
      ./editors/vim/nvim.nix
      ./editors/emacs
      ./editors/vscode.nix

      ./postman.nix
      ./email.nix

      ./browsers/firefox.nix

      ./terminal-emulators/alacritty.nix

      # TODO: Improve my modules. UPDATE: Ditch them entirely, redo them better, I'm currently using `modules` for my (better separated) modules
      myModules.maven
      myModules.mycli

      ./thefuck
      ./git.nix
      ./starship.nix
      ./development.nix
      ./desktop-environment
      ./desktop-environment/screen-lock.nix
      ./keybindings.nix
      ./tmux.nix

      ./shell.nix
      ./virtualization.nix
      modules.kubernetes
    ];

  nixpkgs.config.allowUnfree = true;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.fzf.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = user;
  home.homeDirectory = "/home/${user}";
  home.sessionVariables = {
    EDITOR = "nvim";
    # -X: keep output in terminal
    # -r: preserve colors
    # -F: exit immediately if the entire output fits the screen
    # --mouse --wheel-lines: mouse scroll, speed
    LESS = "-XFr --mouse --wheel-lines=3";
  };

  home.packages = with pkgs; [
    cachix
    xsel # managing Xorg clipboard

    # TODO: any way to just pick-up all packages from scripts?
    scripts.clipscrot # my own script to send screenshot to clipboard
    scripts.bakup # my own script to nicely backup files
    scripts.docker-nuke # cleanup everything docker
    scripts.nix-nuke # cleanup everythin nix

    anki
    zathura # pdf reader
    scrot # making screenshots
    cloc # count lines of code
    pavucontrol
    python3 # TODO: In nixos config?
    ntfs3g # TODO: In nixos config?
    gnupg # TODO somewhere else with the rest of the config
    _1password
    slack
    derivations.soapui57
    inetutils # for telnet (TODO: In cli-essentials.nix?)

    feh # image viewer

    # TODO: Maybe these all in kubernetes-something
    kubernetes-helm
    helmfile
    kustomize

    # TODO: Maybe in virtualization
    podman-compose

    discord

    clang # I just need it to build tree-sitter grammars in emacs

    pgcli

    # TODO configure some emacs client and remove this
    thunderbird
    awscli2
  ] ++ import ./cli-essentials.nix { inherit pkgs; } ;

  programs.maven = {
    enable = true;
    options = {
      "maven.wagon.http.ssl.insecure" = "true";
      "maven.wagon.http.ssl.allowall" = "true";
      "maven.wagon.http.ssl.ignore.validity" = "true";
    };
    settings = secrets.mavenSettings;
  };

  programs.kubernetes = {
    enable = true;
    krew = {
      enable = true;

      plugins = [ 
        "ns"
        "ctx"
        "view-secret"
        "modify-secret"
        "rabbitmq"
      ];
    };
  };

  programs.zoxide = {
    enable = true;
  };


  programs.mpv = {
    enable = true;
    config = {
      save-position-on-quit = true;
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  services.gpg-agent.enable = true;

  programs.lsd = {
    enable = true;
    enableAliases = true;
  };

  programs.ssh = {
    enable = true;
    matchBlocks = secrets.sshMatchBlocks;
    extraConfig = ''
      PubkeyAcceptedKeyTypes +ssh-rsa
    '';
  };

  programs.chromium = {
    enable = true;
    extensions = [
      { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock origin
      { id = "eadndfjplgieldjbigjakmdgkmoaaaoc"; } # xdebug helper
      { id = "jnihajbhpnppcggbcgedagnkighmdlei"; } # livereload
      { id = "aeblfdkhhhdcdjpifhhbdiojplfjncoa"; } # 1password
      { id = "jdkknkkbebbapilgoeccciglkfbmbnfm"; } # apollo client devtools
      { id = "ahfhijdlegdabablpippeagghigmibma"; } # web vitals
    ];
  };

  xdg = {
    enable = true;
  };

  services.dunst.enable = true;

  programs.mycli = {
    enable = true;
    favoriteQueries = secrets.cfQueries;
    multiline = true;
    autoVerticalOutput = true;
  };

  # TODO: put it somewhere else
  services.autorandr.enable = true;
  programs.autorandr = {
    enable = true;
    profiles = 
    let
      laptopScreenFingerprint =  "00ffffffffffff004c835841000000000a1e0104b51d1178020cf1ae523cb9230c505400000001010101010101010101010101010101bc3680b4703820403020880026a51000001bbc3680b4703820403020880026a51000001b0000000f00d1093cd1093c28800000000000000000fe0041544e413333584331312d3020016102030f00e3058000e606050174600700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000b7";
      laptopHdmiFingerprint =  "00ffffffffffff00220e993401010101111c010380351e782a0565a756529c270f5054a10800d1c081c0a9c0b3009500810081800101023a801871382d40582c45000f282100001e000000fd00323c1e5011000a202020202020000000fc004850205648323430610a202020000000ff0036434d383137333959590a20200122020322f149901f04130312021101230907078301000068030c001000002200e2002b023a801871382d40582c45000f282100001e023a80d072382d40102c45800f282100001e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f9";
    in
    {
      portable = {
        fingerprint = {
          "eDP-1" = laptopScreenFingerprint;
        };
        config = {
          "eDP-1" = {
            enable = true;
            primary = true;
            mode = "1920x1080";
          };
        };
      };
      docked = {
        fingerprint = {
          "eDP-1" = laptopScreenFingerprint;
          "HDMI-1" = laptopHdmiFingerprint;
        };

        config = {
          "eDP-1" = {
            enable = true;
            primary = true;
            mode = "1920x1080";
            position = "0x0";
          };
          "HDMI-1" = {
            enable = true;
            mode = "1920x1080";
            position = "1920x0";
          };
        };
      };
    };
  };

  # dark mode
  gtk = {
    enable = true;
    theme = {
      name = "Juno";
      package = pkgs.juno-theme;
    };
  };


  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
