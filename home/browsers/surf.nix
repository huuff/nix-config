{pkgs, ...}:

let
  mydrvs = builtins.fetchGit {
    url = "https://github.com/huuff/derivations.git";
    rev = "9c946a699aaaf234fe00be512391f46f8e05a8e9";
  };
in
{
  imports = [
    "${mydrvs}/surf/home-surf.nix"
  ];

  nixpkgs.overlays = [ (import "${mydrvs}/surf/surf-patches-overlay.nix") ];

  programs.surf = {
    enable = true;
    patches = with pkgs; [ 
      surfPatches.homepage
    ];
  };

}
