{pkgs, ...}:

let
  mydrvs = builtins.fetchGit {
    url = "https://github.com/huuff/derivations.git";
    rev = "f3a2cf75c796db6e10a871af681ba52a18ff7388";
  };
in
{
  imports = [
    "${mydrvs}/st/home-st.nix"
    #../../../derivations/st/home-st.nix # for testing purposes
  ];

  programs.st = {
    enable = true;
    colorscheme = "dracula";
    fontSize = 16;
    scrollback = true;
    blinkingCursor = true;
    #exec = "tmux";
    flags = {
      f = "FiraCode Nerd Font";
    };
  };

}
