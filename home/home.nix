{ pkgs, user, secrets, myModules, ... }:
{
    imports = [
      ./editors/vim/nvim.nix
      ./editors/emacs

      ./browsers/firefox.nix
      # ./browsers/surf.nix

      ./terminal-emulators/st.nix 
      ./terminal-emulators/alacritty.nix

      myModules.scripts

      myModules.maven
      myModules.mycli

      ./thefuck
      ./git.nix
      ./bash.nix
      ./starship.nix
      ./desktop-environment
      ./tmux.nix
    ];

  nixpkgs.config.allowUnfree = true;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.fzf.enable = true;

  haf.scripts.enable = true;

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

  # TODO: make java version more global so I can share it between files
  home.packages = with pkgs; [
    anki
    ((gradleGen.override {
      jdk = jdk11_headless;
      java = jdk11_headless;
    }).gradle_latest)
    zathura
    scrot
    jetbrains.idea-ultimate
    xsel
    cloc
    pavucontrol
    python3
    ntfs3g
    gnupg
    texlive.combined.scheme-full
    keepassx2
    himalaya # not even using it yet
    _1password
    sshfs
    vagrant
    libreoffice
    deluge
    slack
    jetbrains.phpstorm
    tealdeer # tldr command, for quick manpages
    sshpass
    nodePackages.bash-language-server
    translate-shell # very useful! translate text in the shell, as in `trans "hola mundo" es:en -b`
    libxml2 # for xmllint --format

    shellcheck
    shfmt
    nodePackages.diagnostic-languageserver
    postman
    feh

    nixopsUnstable
    nixpkgs-fmt
    rnix-lsp
    nix-prefetch-git
    nixos-shell

    mongodb

    simplescreenrecorder
    teams
  ] ++ import ./cli-essentials.nix { inherit pkgs; } ;

  programs.maven = {
    enable = true;
    options = {
      "maven.wagon.http.ssl.insecure"="true";
      "maven.wagon.http.ssl.allowall"="true";
      "maven.wagon.http.ssl.ignore.validity"="true";
    };
    settings = secrets.mavenSettings;
  };


  programs.mpv = {
    enable = true;
    config = {
      save-position-on-quit = true;
    };
  };

  services.gpg-agent.enable = true;

  programs.lsd = {
    enable = true;
    enableAliases = true;
  };

  programs.ssh = {
    enable = true;
    matchBlocks = secrets.sshMatchBlocks;
  };

  programs.chromium = {
    enable = true;
    extensions = [
      { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock origin
      { id = "eadndfjplgieldjbigjakmdgkmoaaaoc"; } # xdebug helper
      { id = "jnihajbhpnppcggbcgedagnkighmdlei"; } # livereload
    ];
  };

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
