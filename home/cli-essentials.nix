{ pkgs, lib, ... }:
{
  home.packages = with pkgs; [
    python3 # too many scripts I download are in python

    wget
    jq # json inspection
    yq # like jq but for yaml, also provides xq for xml
    git
    devenv

    zip
    unzip

    bc # CLI calculator

    fd # find replacement (also needed for emacs)
    silver-searcher # grep replacement

    btop
    bat # cat replacement
    entr # run command on file changes
    ncdu # see disk usage
    expect # make the non-interactive, interactive
    httpie # curl replacement for web services
    openssl
    libxml2 # for xmllint --format
    up # ultimate plumber: interactively edit pipes
    apacheHttpd # only for using hpasswd
    jwt-cli # decode jwt in the cli with `jwt decode`
    dig
    progress # see progress of cp, mv and dd in the terminal with `watch progress`

    just # command line runner
    libnotify # sending notifications to dunst

    # I use it to shut off my external HDD when unmounting
    # with my eject-disk script
    hdparm

    gh # github

    wl-clipboard # provides wl-copy and wl-paste
  ];

  programs.lsd = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    settings = {
      classic = false;
      blocks = [
        "permission"
        "user"
        "group"
        "size"
        "date"
        "name"
        "git"
      ];
    };
  };

  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  # ignores .gitinignore'd files
  home.sessionVariables.FZF_CTRL_T_COMMAND = "${lib.getExe pkgs.fd} --type f";

  # fzf-tab: replaces zsh completion menu with fzf (e.g. git checkout <TAB>)
  programs.zsh.plugins = [
    {
      name = "fzf-tab";
      src = "${pkgs.zsh-fzf-tab}/share/fzf-tab";
    }
  ];
}
