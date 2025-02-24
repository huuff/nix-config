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
          ./libs.el
          ./util.el
          ./compilation.el
          ./window-management.el

          (pkgs.substituteAll { 
            src = ./init.el;
            # XXX: dape (for debugging) needs the vscode LLDB adapter
            # so I just substitute it into the elisp from here
            codelldb = "${pkgs.vscode-extensions.vadimcn.vscode-lldb}/share/vscode/extensions/vadimcn.vscode-lldb/adapter/codelldb";
          })

          ./safe-locals.el

          ./tabs.el

          ./multicursor.el
          ./keybindings.el
        ]);

        defaultInitFile = true;

        package = pkgs.emacs-unstable.override {
          withTreeSitter = true;
          withNativeCompilation = true;
          withImageMagick = true;
        };
        alwaysEnsure = true;

        extraEmacsPackages = epkgs: [
          (pkgs.callPackage ./eglot-booster.nix { inherit pkgs epkgs; })
        ];
      })
    )
  ];

  home.file = {
    ".emacs.d/early-init.el".text = builtins.readFile ./early-init.el;
    ".emacs.d/templates".text = builtins.readFile ./templates.eld;
  };
}
