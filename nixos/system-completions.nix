{ }:
{
  # TODO: /share doesn't even edxist? What's this for? remove it?
  # Apparently, completion of shells installed through home-manager (bash, zsh) needs linking of their
  # completions dir to be able to work with system packages (such as systemctl).
  environment.pathsToLink = [
    "/share/bash-completion"
    "/share/zsh"
  ];
}
