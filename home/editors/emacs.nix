{ pkgs, emacs-overlay, ... }:

{
  nixpkgs.overlays = [ emacs-overlay.overlay ];

  programs.emacs = {
    enable = true;

    extraPackages = epkgs: [
      epkgs.evil # vim keybindings emulation
      epkgs.use-package # easy package config
      epkgs.bind-key # key binding
      epkgs.dracula-theme # theme
      # TODO: Put the command to install icons
      epkgs.all-the-icons # icon pack
      epkgs.treemacs # side-drawer file explorer

      # Language-specific modes
      epkgs.nix-mode
      epkgs.rustic

      # LSP
      epkgs.lsp-mode
      epkgs.lsp-ui # I actually don't know what it does
      epkgs.lsp-treemacs
      epkgs.helm-lsp

      # Universally loved packages
      epkgs.company
      epkgs.flycheck
      epkgs.helm
      # FUTURE: emacs29 has a native integration of tree-sitter
      # so this shouldn't be needed
      # TODO: rust not working! ABI is supposedly "too new"
      epkgs.tree-sitter
      epkgs.tree-sitter-langs
    ];

    extraConfig = ''
      ;; disable ugly top toolbars and scroll bars
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (menu-bar-mode -1)

      ;; show line numbers in programming modes
      (add-hook 'prog-mode-hook 'display-line-numbers-mode)

      ;; (use-package)
      (eval-when-compile
        (require 'use-package))

      ;; (bind-key) necessary for use-package
      (use-package bind-key
        :ensure t
        :config
        (add-to-list 'same-window-buffer-names "*Personal Keybindings*")
      )

      ;; (helm)
      (use-package helm
        :bind (
          ("M-x" . helm-M-x)
        )
      )

      ;; (tree-sitter) FUTURE: It's supposedly included in emacs29
      (use-package tree-sitter
        :config
        (require 'tree-sitter-langs)
        (global-tree-sitter-mode)
        ;; TODO: Use :hook?
        (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
      )

      ;; (dracula)
      (load-theme 'dracula t)


      ;; (nix-mode)
      (use-package nix-mode
        :mode "\\.nix\\'"
      )

      ;; (flycheck)
      (use-package flycheck
      )

      ;; (lsp-mode)
      (use-package lsp-mode
        :init
          (setq lsp-use-plists nil)
        :hook (
          (prog-mode . lsp)
        )
        :commands lsp
      )

      (use-package lsp-ui :commands lsp-ui-mode)
      (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
      (use-package helm-lsp :commands helm-lsp-workspace-symbol)


      ;; (treemacs)
      (use-package treemacs
        :ensure t
        :defer t
        :hook (emacs-startup . treemacs)
      )


      ;; (all-the-icons) necessary for neotree
      (use-package all-the-icons
        :if (display-graphic-p))

      ;; Setup evil
      (require 'evil)
      (evil-mode 1)

      ;; rust
      (use-package rustic)
    '';
  };
}
