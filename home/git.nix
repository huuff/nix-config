{ pkgs, secrets, ... }:
{
  programs.git = {
    enable = true;
    userName = "Haf";
    userEmail = "haf@protonmail.ch";
    ignores = [ 
      "*~"
      "*.swp" # vim swap files 
      "texfrag" # trash generated by texfrag in emacs
      "\\#*\\#" # emacs swap files
      "/result/" # nix outputs
      "/result-data/" # something related to nix output
    ];
    extraConfig = {
      credential.helper = "cache --timeout=3600";
      init.defaultBranch = "master";
      pull.rebase = "false";
    };

    includes = [
      {
        condition = "gitdir:~/work/";
        contents = secrets.gitWorkConfig;
      }
    ];

    delta = {
      enable = true;
      options = {
        line-numbers = true;
      };
    };

    aliases = {
      co = "checkout";
      ss = "status";
      cm = "commit -m";
    };
  };

}
