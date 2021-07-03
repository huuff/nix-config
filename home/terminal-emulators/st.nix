{pkgs, ...}:

let
  mydrvs = builtins.fetchGit {
    url = "https://github.com/huuff/derivations.git";
    rev = "4bd4e3fb73d6532fa557516cdd67e3948810d4c1";
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
    flags = {
      f = "FiraCode Nerd Font";
    };
  };

}
