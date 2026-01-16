{
  pkgs,
  user,
  derivations,
  modules,
  scripts,
  ...
}:
{
  imports = [
    ./editors/vim/nvim.nix
    ./editors/emacs
    ./editors/vscode.nix

    ./browsers/firefox.nix
    ./browsers/chromium.nix

    ./terminal-emulators/alacritty.nix

    ./git.nix
    ./prompt.nix
    ./development.nix
    ./desktop-environment

    ./shell.nix
    ./virtualization.nix
    modules.kubernetes

    ./cloud.nix
    ./secrets.nix
    ./llm
    ./ssh.nix
    ./cli-essentials.nix
    ./slack.nix

    ./crypto.nix
  ];

  nixpkgs.config.allowUnfree = true;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

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
    # this makes tauri apps not look like shit
    XDG_DATA_DIRS = "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}:$XDG_DATA_DIRS";
  };

  home.packages =
    with pkgs;
    [
      irust

      cachix
      devenv

      zathura # pdf reader
      scrot # making screenshots
      cloc # count lines of code
      pavucontrol
      ntfs3g # TODO: In nixos config?

      _1password-cli
      soapui
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
      tor-browser
      libreoffice
      hoppscotch

    ]
    ++ lib.attrValues scripts;

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

  xdg.enable = true;

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
