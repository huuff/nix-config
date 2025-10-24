{ pkgs, ... }:
{
  home.packages = with pkgs; [
    wget
    jq # json inspection
    yq # like jq but for yaml, also provides xq for xml
    git

    zip
    unzip

    bc # CLI calculator

    fd # find replacement (also needed for emacs)
    silver-searcher # grep replacement

    fzf # TODO: Is it necessary here? I think it's enough to enable it through home-manager
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
      blocks = [ "permission" "user" "group" "size" "date" "name" "git" ];
    };
  };
}
