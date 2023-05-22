{ pkgs, ... }: with pkgs;
[
  wget
  jq # json inspection
  yq # like jq but for yaml, also provides xq for xml
  git

  zip
  unzip

  fd # find replacement
  silver-searcher # grep replacement
  fzf # TODO: Is it necessary here? I think it's enough to enable it through home-manager
  htop
  bat # cat replacement
  lsd # ls replacement
  entr # run command on file changes
  ncdu # see disk usage
  expect # make the non-interactive, interactive
  comma # run commands without installing e.g. `, cowsay hello`
  httpie # curl replacement for web services
  libressl # openssl alternative, tools for x509 management and inspection
  libxml2 # for xmllint --format
  up # ultimate plumber: interactively edit pipes
  apacheHttpd # only for using hpasswd
  jwt-cli # decode jwt in the cli with `jwt decode`
  dig
  progress # see progress of cp, mv and dd in the terminal with `watch progress`

  just # command line runner
  libnotify # sending notifications to dunst
]
