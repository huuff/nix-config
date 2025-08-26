{ pkgs, lib, user, secrets, myModules, homeModules, derivations, modules, scripts, ... }:
{
  imports = [
    ./editors/vim/nvim.nix
    ./editors/emacs
    ./editors/vscode.nix

    ./postman.nix
    ./email.nix

    ./browsers/firefox.nix
    ./browsers/chromium.nix

    ./terminal-emulators/alacritty.nix

    # TODO: Improve my modules. UPDATE: Ditch them entirely, redo them better, I'm currently using `modules` for my (better separated) modules
    myModules.maven
    myModules.mycli

    ./git.nix
    ./starship.nix
    ./development.nix
    ./desktop-environment
    ./desktop-environment/screen-lock.nix
    ./keybindings.nix

    ./shell.nix
    ./virtualization.nix
    modules.kubernetes

    ./cloud.nix
    ./secrets.nix
    ./llm.nix
  ] ++ lib.attrValues homeModules;

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
    # TODO: remove the flag comments and use the longform which should be self-explanatory
    # -X: keep output in terminal
    # -r: preserve colors
    # -F: exit immediately if the entire output fits the screen
    # --mouse --wheel-lines: mouse scroll, speed
    LESS = "-XFr --mouse --wheel-lines=3";
  };

  home.packages = with pkgs; [
    irust

    cachix
    xsel # managing Xorg clipboard

    # TODO: any way to just pick-up all packages from scripts?
    scripts.clipscrot # my own script to send screenshot to clipboard
    scripts.pathscrot # send screenshots to a file and path to clipboard (I use it for claude code)
    scripts.bakup # my own script to nicely backup files
    scripts.docker-nuke # cleanup everything docker
    scripts.nix-nuke # cleanup everything nix
    scripts.nuke-all #cleanup everything everything

    anki
    zathura # pdf reader
    scrot # making screenshots
    cloc # count lines of code
    pavucontrol
    ntfs3g # TODO: In nixos config?
    gnupg # TODO somewhere else with the rest of the config
    _1password-cli
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

    tor-browser
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
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  programs.ssh = {
    enable = true;
    matchBlocks = secrets.sshMatchBlocks;
    extraConfig = ''
      PubkeyAcceptedKeyTypes +ssh-rsa
    '';
  };

  xdg.enable = true;

  services.dunst.enable = true;

  programs.mycli = {
    enable = true;
    favoriteQueries = secrets.cfQueries;
    multiline = true;
    autoVerticalOutput = true;
  };


  # dark mode
  gtk = {
    enable = true;
    theme = {
      name = "Arc-Dark";
      package = pkgs.arc-theme;
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
    cursorTheme = {
      name = "Adwaita"; # A good default, or choose another like Bibata
      package = pkgs.adwaita-icon-theme;
      size = 24;
    };
  };

  # Keeps screen on when playing on fullscreen
  services.caffeine.enable = true;

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
