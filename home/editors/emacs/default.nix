{ pkgs, emacs-overlay, ... }:

{
  nixpkgs.overlays = [ 
    emacs-overlay.overlay
  ];

  home.packages = [
    (
      (pkgs.emacsWithPackagesFromUsePackage {
        config = ./init.el;

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

        # XXX: I'm using the old tree-sitter plugin because the
        # native integration in emacs29 doesn't please me
        # but it's old and not updated, so the nixpkgs tree-sitter-grammars are not compatible
        # therefore, I just recompile them with an older ABI
        override = final: prev:
        {
          tree-sitter-grammars = prev.tree-sitter-grammars // {
            tree-sitter-rust = prev.tree-sitter-grammars.tree-sitter-rust.overrideAttrs (_: {
              nativeBuildInputs = [ final.nodejs final.tree-sitter ];
              configurePhase = ''
                tree-sitter generate --abi 13 src/grammar.json
              '';
            });
          };
        };
      })
      )
    ];
}
