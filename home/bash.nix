{ pkgs, ... }:

{
  programs.bash = {
    enable = true;

    shellAliases = {
      perms = ''stat -c "%a %n"'';
      ports = ''netstat -tulpn | grep LISTEN'';
      trace-net = ''strace -f -e trace=network'';
      sudo= "sudo "; # https://askubuntu.com/questions/22037/aliases-not-available-when-using-sudo/22043#22043 
      oplogin = "eval $(op signin my)";
      agip = ''ag "\b\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\b"'';
    };

    initExtra = ''
      # Necessary so Nix works on non-NixOS
      export NIX_PATH=$HOME/.nix-defexpr/channels''${NIX_PATH:+:}$NIX_PATH

      # History related
      export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
      export HISTSIZE=100000                   # big big history
      export HISTFILESIZE=100000               # big big history
      shopt -s histappend                      # append to history, don't overwrite it
      # Save and reload the history after each command finishes
      export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

      # Start thefuck
      eval $(thefuck --alias)
    '';
  };

}
