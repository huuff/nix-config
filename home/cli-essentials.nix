{ pkgs, ... }: with pkgs;
[
  jq
  git
  zip
  unzip
  fd # find replacement
  ag # grep replacement
  fzf
  htop
  bat # cat replacement
  lsd # ls replacement
  entr
  ncdu # see disk usage
]
