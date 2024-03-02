{ pkgs, emacs-overlay, ... }:

{
  nixpkgs.overlays = [ 
    emacs-overlay.overlay
  ];

  home.packages = [
    (
      (pkgs.emacsWithPackagesFromUsePackage {
        config = builtins.concatStringsSep "\n" (map builtins.readFile [ 
            ./util.el
            ./compilation.el
            ./windows.el

            # XXX: dape (for debugging) needs the vscode LLDB adapter
            # so I just substitute it into the elisp from here
            (pkgs.substituteAll { 
              src = ./init.el;
              codelldb = "${pkgs.vscode-extensions.vadimcn.vscode-lldb}/share/vscode/extensions/vadimcn.vscode-lldb/adapter/codelldb";
            })
            ./tabs.el

            ./hydra.el
            ./multicursor.el
        ]);

        defaultInitFile = true;

        package = pkgs.emacs-unstable.override {
          withTreeSitter = true;
          withNativeCompilation = true;
        };
        alwaysEnsure = true;
      })
      )
    ];
  }
