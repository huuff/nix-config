{ pkgs, emacs-overlay, ... }:

# TASKS:
# TODO: Some autopair stuff (smartparens)
# TODO: Some vim command emulation
# TODO: Reorganize it a little
# TODO: Try to have all packages in use-package
# TODO: Use emacs-overlay feature to install packages from use-package?
# TODO: Auto install icons
# TODO: Centaur tabs looks fuckugly, there are no icons, for example. Maybe I need a compatible theme?
# TODO: configure Helm for more features (currently it's only for M-x)
# TODO: Some leader key configuration?
# TODO: Use projectile
# TODO: Make helm open in a small buffer rather than taking the whole fucking buffer. UPDATE: I'm doing it, but maybe I can do it with popwin?
{
  nixpkgs.overlays = [ 
    emacs-overlay.overlay
    # XXX: I'm using the old tree-sitter plugin because the
    # native integration in emacs29 doesn't please me
    # but it's old and not updated, so the nixpkgs tree-sitter-grammars are not compatible
    # therefore, I just recompile them with an older ABI
    (final: prev:
    {
      tree-sitter-grammars = prev.tree-sitter-grammars // {
        tree-sitter-rust = prev.tree-sitter-grammars.tree-sitter-rust.overrideAttrs (_: {
          nativeBuildInputs = [ final.nodejs final.tree-sitter ];
          configurePhase = ''
            tree-sitter generate --abi 13 src/grammar.json
          '';
        });
      };
    })
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-unstable.override {
      withTreeSitter = true;
    };

    extraPackages = epkgs: [
      epkgs.evil # vim keybindings emulation
      epkgs.use-package # easy package config
      epkgs.bind-key # key binding
      epkgs.dracula-theme # theme
      # TODO: Put the command to install icons or just install them automatically
      epkgs.all-the-icons # icon pack
      epkgs.treemacs # side-drawer file explorer
      epkgs.treemacs-evil # without it, left click doesn't work

      # Language-specific modes
      epkgs.nix-mode
      epkgs.rustic
      epkgs.rust-mode

      # LSP
      epkgs.lsp-mode
      epkgs.lsp-ui # I actually don't know what it does
      epkgs.lsp-treemacs
      epkgs.helm-lsp

      # Universally loved packages
      epkgs.company
      epkgs.flycheck
      epkgs.helm

      epkgs.tree-sitter
      epkgs.tree-sitter-langs

      epkgs.centaur-tabs # tabs

      epkgs.popwin
    ];

    extraConfig = ''
      ;; disable ugly top toolbars and scroll bars
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (menu-bar-mode -1)

      ;; show line numbers in programming modes
      (add-hook 'prog-mode-hook 'display-line-numbers-mode)

      ;; set font to 11pt (height is in 1/10pt so 110 = 11pt)
      (set-face-attribute 'default nil :height 110)

      ;; allow pasting with Ctrl+V, even in minibuffer
      (global-set-key (kbd "C-v") 'yank)

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
        ;; TODO: Why can't I config this in a :custom block?
        :config
          (setq 
            helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
            helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
            helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
            helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
            helm-ff-file-name-history-use-recentf t
            helm-echo-input-in-header-line t
          )
          (setq helm-autoresize-max-height 0)
          (setq helm-autoresize-min-height 20)
          (helm-autoresize-mode 1)
          (helm-mode 1)
      )

      ;; (company)
      ;; TODO: Use LSP recommended config (prefix 0)
      (use-package company
        :ensure t
        :config
          (setq
            company-minimum-prefix-length 2
          )
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
      (use-package treemacs-evil)


      ;; (all-the-icons) 
      (use-package all-the-icons
        :if (display-graphic-p))

      ;; (evil)
      (require 'evil)
      (evil-mode 1)

      ;; rust
      (use-package rustic)
      (use-package rust-mode)

      ;; FUTURE: emacs29 is supposed to have a native, better integration with tree-sitter
      ;; but it gives me a lot of troubles that I hope will be resolved by emacs30
      ;; but for now, this is the ancient way of doing it
      ;; (tree-sitter)
      (use-package tree-sitter
        :config
        (require 'tree-sitter-langs)
        (global-tree-sitter-mode)
        ;; TODO: Use :hook?
        (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
      )

      ;; (centaur-tabs)
      (use-package centaur-tabs
        :demand
        :config
          (centaur-tabs-mode t)
          (centaur-tabs-headline-match)
      )

      ;; (popwin)
      (use-package popwin
        :config
          (popwin-mode 1)
          ;; cargo commands for rustic
          (push '("^\*cargo-.+\*$" :regexp t) popwin:special-display-config)
      )
    '';
  };
}
