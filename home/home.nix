{ pkgs, user, secrets, myModules, ... }:
{
    imports = [
      ./editors/vim/nvim.nix

      ./browsers/firefox.nix

      ./terminal-emulators/alacritty.nix

      # TODO: Improve my modules
      myModules.scripts
      myModules.maven
      myModules.mycli

      ./thefuck
      ./git.nix
      ./bash.nix
      ./starship.nix
      ./development.nix
      ./desktop-environment
      ./tmux.nix
      ./email.nix
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

  home.packages = with pkgs; [
    anki
    zathura
    scrot
    xsel
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
    tealdeer # tldr command, for quick manpages
    translate-shell # very useful! translate text in the shell, as in `trans "hola mundo" es:en -b`
    libxml2 # for xmllint --format

    postman
    feh

    nix-prefetch-git
    nixos-shell
    mongodb-4_2 # a good 4-5 hours compilation time on my slowest computer, so plan ahead TODO: Self-host a binary cache

    kubectl
    kubernetes-helm
    helmfile
    kustomize

    vagrant
    podman-compose
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

  programs.zoxide = {
    enable = true;
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
    ];
  };

  programs.mycli = {
    enable = true;
    favoriteQueries = secrets.cfQueries;
    multiline = true;
    autoVerticalOutput = true;
  };

  programs.vscode = {
    enable = true;
    keybindings = [ # Some keybindings as in Intellij IDEA
      {
        key = "ctrl+alt+l";
        command = "editor.action.formatDocument";
      }

      {
        key = "ctrl+alt+s";
        command = "workbench.action.files.saveAll";
      }
    ];
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
