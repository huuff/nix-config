{pkgs, ...}:

let
  mydrvs = builtins.fetchGit {
    url = "https://github.com/huuff/derivations.git";
    rev = "3c6ce143ad885d7cc44347541a414962cca45f91";
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
    exec = "tmux";
    flags = [
      {f = "FiraCode Nerd Font";}
    ];
  };

}
