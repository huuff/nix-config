{ pkgs, ... }: with pkgs;
[
  jq
  yq # like jq but for yaml, also provides xq for xml
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
  expect # make the non-interactive, interactive
]
