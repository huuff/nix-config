{ pkgs, emacs-overlay, ... }:

# TASKS:
# TODO: Entire buffer textobj would be nice, I do `cae` or `dae` a lot in vim
# TODO: Seems like lsp-ui lens diagnostics might break a lot of stuff when moving!
# TODO: LSP code actions don't seem to actually be using helm
# TODO: Some cool way of showing available code actions
# TODO: Some keybindings to cycle through flycheck list diagnostics. UPDATE: Or maybe use lsp-treemacs-errors-list
# TODO: Comment and explain ALL packages
# TODO: Reorganize it a little
# TODO: There are two commands I need to run so fonts work. Is there anyway I could automate it or notify whether it's needed?:
  # - nerd-icons-install-fonts
  # - all-the-icons-install-fonts
# TODO: configure Helm for more features (currently it's only for M-x)
# TODO: Use magit
# TODO: Search MELPA for any packages that contain the names of any packages I use, see if there are any more integrations I'm missing!
# TODO: Set correct dependencies between packages with use-package (:after)
# TODO: DAP mode for debugging
# TODO: Use expand-region
# TODO: Multi-cursor stuff
# TODO: A mouse hover pop-up for flycheck would be nice
# TODO: Indent guides for YAML
# TODO: Maybe I could setup flycheck-inline instead of lsp-ui for rust-mode? UPDATE: I've currently disabled lsp-ui-sideline and I'm using flycheck-inline, but I'm not sure whether it's generally the best solution... maybe I could just do it for rust, or use lsp-ui-sideline for code actions?
# TODO: Keybinding to close all other tabs with centaur
# TODO: Maybe I should use eglot since it's inbuilt into emacs... besides, I have many issues with inlay hints (see below)
# TODO: lsp-mode inlay hints are pretty unwieldy... they are constantly appearing and disappearing while I write in insert mode, moving my text. I've disabled it, but it's such a nice feature, I'd like to find a way to have it enabled
# TODO: Maybe I should use popper.el instead of popwin.el
# TODO: I don't think tree-sitter-mode is even working... emacs starts out with no highlighting and only appears when I disable and re-enable tree-sitter-hl-mode
{
  nixpkgs.overlays = [ 
    emacs-overlay.overlay
  ];

  home.packages = with pkgs; [
    (
      (pkgs.emacsWithPackagesFromUsePackage {
        config = ./emacs.el;

        defaultInitFile = true;

        package = pkgs.emacs-unstable.override {
          withTreeSitter = true;
          withNativeCompilation = true;
        };
        alwaysEnsure = true;
        extraEmacsPackages = epkgs: [
          pkgs.python3
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

  #programs.emacs = {

    #extraPackages = epkgs: [
      #epkgs.use-package # easy package config
      ## needed for advanced features of treemacs for git
      #pkgs.python3
    #];

  #};
}
