{ pkgs, ... }: with pkgs;
[
  jq
  yq # like jq but for yaml, also provides xq for xml
  git
  zip
  unzip
  fd # find replacement
  silver-searcher # grep replacement
  fzf
  htop
  bat # cat replacement
  lsd # ls replacement
  entr # run command on file changes
  ncdu # see disk usage
  expect # make the non-interactive, interactive
]
