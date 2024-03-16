{ pkgs, emacs-overlay, ... }:

{
  nixpkgs.overlays = [ 
    emacs-overlay.overlay
  ];

  home.packages = [
    pkgs.emacs-lsp-booster
    (
      (pkgs.emacsWithPackagesFromUsePackage {
        config = builtins.concatStringsSep "\n" (map builtins.readFile [ 
            ./util.el
            ./compilation.el
            ./window-management.el

            (pkgs.substituteAll { 
              src = ./init.el;
              # XXX: dape (for debugging) needs the vscode LLDB adapter
              # so I just substitute it into the elisp from here
              codelldb = "${pkgs.vscode-extensions.vadimcn.vscode-lldb}/share/vscode/extensions/vadimcn.vscode-lldb/adapter/codelldb";
              # XXX: some packages are not on ELPA nor MELPA, and I don't think straight works in nixos,
              # so I just clone them into ./snatches and then add them to the load path
              snatches = ./snatches;
            })
            ./tabs.el

            ./multicursor.el
            ./keybindings.el
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
