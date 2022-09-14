{ modules, config, pkgs, ... }:
{
  imports = [
    modules.shell
  ];

  programs.shell = {
    aliases = {
      perms = ''stat -c "%a %n"'';
      trace-net = ''strace -f -e trace=network''; # is it useful?
      oplogin = "eval $(op signin --account my.1password.com)";
      agip = ''ag "\b\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\b"'';
      ssh = "TERM=xterm-256color command ssh"; # for some servers that don't accept my terminals
      syss = "systemctl status";
      sysf = "systemctl --failed";
      sysj = "systemctl list-jobs";
      sysr = "systemctl restart";
      sysc = "systemctl cat";
      jrn = "journalctl -u";
      jrnf = "journalctl -fu";
      sudo= "sudo "; # https://askubuntu.com/questions/22037/aliases-not-available-when-using-sudo/22043#22043 
      watch = "watch "; # same as sudo
      vssh = "TERM=xterm-256color vagrant ssh";
      vup = "vagrant up";
      vhalt = "vagrant halt";
      vss = "vagrant global-status";
      cloc = "cloc --vcs=git";
      ag2 = "ag --context=2";
      duh = "du -h --max-depth=1";
      k = "kubectl";
      hf = "helmfile";
      hm = "himalaya";
      xmlfmt = "xmllint --format -";
      nix-shell = "nix shell"; # Prevent me from ever using nix-shell, since it's older and I'm too used to it
    };

    scriptDir = "/$HOME/scripts";

    completionsDir = {
      bash = "${config.home.homeDirectory}/shell-completions/bash";
      fish = "${config.home.homeDirectory}/shell-completions/fish";
      zsh = "${config.home.homeDirectory}/shell-completions/zsh";
    };

  };

    ### FISH
    programs.fish = {
      enable = true;

      plugins = [
        {
          # Allow !! and !$ history substitution like in POSIX
          name = "bang-bang";
          src = pkgs.fetchFromGitHub {
            owner = "oh-my-fish";
            repo = "plugin-bang-bang";
            rev = "f969c618301163273d0a03d002614d9a81952c1e";
            sha256 = "1r3d4wgdylnc857j08lbdscqbm9lxbm1wqzbkqz1jf8bgq2rvk03";
          };
        }
      ];
    };

    ### BASH
    programs.bash = {
      enable = true;

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

        # To allow installing node packages globally
        export PATH=~/.npm-packages/bin:$PATH
        export NODE_PATH=~/.npm-packages/lib/node_modules
      '';
    };

    ### ZSH
    programs.zsh = {
      enable = true;

      enableAutosuggestions = true;
      enableSyntaxHighlighting = true;

      defaultKeymap = "emacs";

      # Move across words with Ctrl + Left/Right
      initExtra = ''
        bindkey "^[[1;5C" forward-word
        bindkey "^[[1;5D" backward-word
      '';
    };
  }
