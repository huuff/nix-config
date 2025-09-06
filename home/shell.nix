{ modules, config, ... }:
{
  imports = [
    modules.shell
  ];

  programs.shell = {
    aliases = {
      op-login = "eval $(op signin --account my.1password.com)";
      ssh = "TERM=xterm-256color command ssh"; # for some servers that don't accept my terminals
      sudo= "sudo "; # https://askubuntu.com/questions/22037/aliases-not-available-when-using-sudo/22043#22043 
      watch = "watch "; # same as sudo
      cloc = "cloc --vcs=git";
      ag2 = "ag --context=2";
      agi = "ag --ignore '*.sql' --ignore '*.svg' --ignore '*.afm' --ignore '*.ser' --ignore '*.ufm'";
      duh = "du -h --max-depth=1";
      k = "kubectl";
      hf = "helmfile";
      xmlfmt = "xmllint --format -";
      # disable emacs splash screen, cannot do it from config file, so this will do
      emacs = "emacs --no-splash";
    };

    scriptDir = "$HOME/scripts";

    completionsDir = {};
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

        # emacs' EAT shell integration
        [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
          source "$EAT_SHELL_INTEGRATION_DIR/bash"
      '';
    };

    ### ZSH
    programs.zsh = {
      enable = true;

      autosuggestion.enable = true;
      syntaxHighlighting = {
        enable = true;
      };

      defaultKeymap = "emacs";

      # TODO: Check if some of these settings would be nice to improve my history
      # https://unix.stackexchange.com/a/273863/483246
      initContent = ''
        # Move across words with Ctrl + Left/Right
        bindkey "^[[1;5C" forward-word
        bindkey "^[[1;5D" backward-word

        # increase history size
        export HISTSIZE=100000
        export SAVEHIST=100000

        # use ^X^E to edit command in $EDITOR
        autoload -z edit-command-line
        zle -N edit-command-line
        bindkey "^X^E" edit-command-line

        # emacs' EAT shell integration
        [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
          source "$EAT_SHELL_INTEGRATION_DIR/zsh"
      '';
    };
  }
