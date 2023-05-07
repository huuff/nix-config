{ pkgs, user, secrets, myModules, derivations, modules, ... }:
{
    imports = [
      ./editors/vim/nvim.nix
      ./editors/vscode.nix

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
      ./tmux.nix
      ./email.nix

      ./shell.nix
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
    # I need both xsel and xclip because...
    # even though xsel is supposed to be better, I can't
    # copy with type (-t in xclip) with it, so I can't use my
    # clipscrot script
    xsel
    xclip

    anki
    zathura
    scrot
    cloc
    pavucontrol
    python3 # TODO: In nixos config?
    ntfs3g # TODO: In nixos config?
    gnupg
    texlive.combined.scheme-full
    _1password
    sshfs
    deluge
    slack
    translate-shell # very useful! translate text in the shell, as in `trans "hola mundo" es:en -b`
    derivations.soapui57
    inetutils # for telnet (TODO: In cli-essentials.nix?)
    commitizen # command for increasing versions

    postman
    feh

    nix-prefetch-git
    nixos-shell

    kubernetes-helm
    helmfile
    kustomize

    vagrant
    podman-compose

    spotify 
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
