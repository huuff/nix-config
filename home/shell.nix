{ modules, config, pkgs, ... }:
{
  imports = [
    modules.shell
  ];

  programs.shell = {
    aliases = {
      perms = ''stat -c "%a %n"'';
      trace-net = ''strace -f -e trace=network''; # is it useful?
      op-login = "eval $(op signin --account my.1password.com)";
      ssh = "TERM=xterm-256color command ssh"; # for some servers that don't accept my terminals
      l = "ls";
      sudo= "sudo "; # https://askubuntu.com/questions/22037/aliases-not-available-when-using-sudo/22043#22043 
      watch = "watch "; # same as sudo
      cloc = "cloc --vcs=git";
      ag2 = "ag --context=2";
      agi = "ag --ignore '*.sql' --ignore '*.svg' --ignore '*.afm' --ignore '*.ser' --ignore '*.ufm'";
      duh = "du -h --max-depth=1";
      k = "kubectl";
      hf = "helmfile";
      hm = "himalaya";
      xmlfmt = "xmllint --format -";
      nix-shell = "nix shell"; # Prevent me from ever using nix-shell, since it's older and I'm too used to it
      # disable emacs splash screen, cannot do it from config file, so this will do
      emacs = "emacs --no-splash";
    };

    scriptDir = "$HOME/scripts";

    completionsDir = {
      bash = "${config.home.homeDirectory}/shell-completions/bash";
      #fish = "${config.home.homeDirectory}/shell-completions/fish";
      zsh = "${config.home.homeDirectory}/shell-completions/zsh";
    };

  };

    # TODO: Removed fish because it was crashing for some reason
    ### FISH
    #programs.fish = {
      #enable = true;

      #plugins = [
        #{
          ## Allow !! and !$ history substitution like in POSIX
          #name = "bang-bang";
          #src = pkgs.fetchFromGitHub {
            #owner = "oh-my-fish";
            #repo = "plugin-bang-bang";
            #rev = "f969c618301163273d0a03d002614d9a81952c1e";
            #sha256 = "1r3d4wgdylnc857j08lbdscqbm9lxbm1wqzbkqz1jf8bgq2rvk03";
          #};
        #}
      #];
    #};

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

      enableAutosuggestions = true;
      syntaxHighlighting = {
        enable = true;
      };

      defaultKeymap = "emacs";

      # Move across words with Ctrl + Left/Right
      initExtra = ''
        bindkey "^[[1;5C" forward-word
        bindkey "^[[1;5D" backward-word

        # emacs' EAT shell integration
        [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
          source "$EAT_SHELL_INTEGRATION_DIR/zsh"
      '';
    };
  }
