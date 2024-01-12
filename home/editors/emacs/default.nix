{ pkgs, emacs-overlay, ... }:

{
  nixpkgs.overlays = [ 
    emacs-overlay.overlay
  ];

  home.packages = [
    (
      (pkgs.emacsWithPackagesFromUsePackage {
        config = builtins.concatStringsSep "\n" (map builtins.readFile [ 
            ./init.el 
            ./hydra.el
        ]);

        defaultInitFile = true;

        package = pkgs.emacs-unstable.override {
          withTreeSitter = true;
          withNativeCompilation = true;
        };
        alwaysEnsure = true;
        extraEmacsPackages = epkgs: [
          # needed for some advanced features of treemacs
          pkgs.python3
          # for consult-fd
          pkgs.fd
          # for consult-ripgrep
          pkgs.ripgrep
        ];
      })
      )
    ];
  }