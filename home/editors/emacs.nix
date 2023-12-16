{ pkgs, emacs-overlay, ... }:

# TASKS:
# TODO: Use evil-collection?
# TODO: Missing python3! Else some treemacs features wont work
# TODO: Install yasnippet (LSP is asking for it)
# TODO: Some vim command emulation
# TODO: Reorganize it a little
# TODO: Try to have all packages in use-package
# TODO: Use emacs-overlay feature to install packages from use-package?
# TODO: Auto install icons
# TODO: configure Helm for more features (currently it's only for M-x)
# TODO: Prefixes for all my commands? Like I have SPC p for projectile
# TODO: Use magit
# TODO: Search MELPA for any packages that contain the names of any packages I use, see if there are any more integrations I'm missing!
# TODO: Maybe some nice status line?
# TODO: DAP mode for debugging
# TODO: vim-like (evil?) config for tab switching for centaur tabs
# TODO: Try to set this up with elisp instead of nix
# TODO: Use expand-region
# TODO: A mouse hover pop-up for flycheck would be nice
# TODO: flycheck-rust is apparently good
let
  leader-key = "SPC";
  lsp-key = "l";
in
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
      # TODO: Put the command to install icons or just install them automatically
      epkgs.all-the-icons # icon pack
      epkgs.treemacs # side-drawer file explorer
      epkgs.treemacs-evil # without it, left click doesn't work
      epkgs.treemacs-projectile

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
      epkgs.smartparens

      epkgs.projectile
      epkgs.helm-projectile
      epkgs.which-key

      epkgs.doom-themes
      epkgs.general
    ];

    extraConfig = ''
      ;; disable ugly top toolbars and scroll bars
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (menu-bar-mode -1)

      ;; show line numbers in programming modes
      (add-hook 'prog-mode-hook 'display-line-numbers-mode)

      ;; set font 
      (set-frame-font "Fira Code 10" nil t)

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
          ;; enables descriptive labels in which-key for lsp
          (lsp-mode . lsp-enable-which-key-integration)
        )
        :config
          ;; don't know why but only these two commands
          ;; will make lsp work with leader key and general.el
          (setq lsp-keymap-prefix "${leader-key} ${lsp-key}")
          (fset 'lsp-command-map lsp-command-map)
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
      ;; TODO: Use use-package
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
          (setq centaur-tabs-set-icons t)
          (setq centaur-tabs-gray-out-icons 'buffer)
          (setq centaur-tabs-set-bar 'left)
          (setq centaur-tabs-set-modified-marker t)
      )

      ;; (popwin)
      (use-package popwin
        :config
          (popwin-mode 1)
          ;; cargo (rustic)
          (push '("^\*cargo-.+\*$" :regexp t) popwin:special-display-config)

          ;; helm 
          (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
          (push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)

          ;; flycheck
          (push "*Flycheck errors*" popwin:special-display-config)
      )

      ;; TODO: This is giving some weird error when building (with nix, I'll have to read on how to find these logs)
      ;; what do I do?
      ;; (smartparens)
      (use-package smartparens-mode
        :ensure smartparens
        :hook 
          (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
        :config
          ;; load default config
          (require 'smartparens-config)
      )

      ;; (projectile)
      (use-package projectile
        :config
        (projectile-mode +1)
      )
      (use-package treemacs-projectile)
      (use-package helm-projectile
        :config
        (helm-projectile-on)
      )

      ;; (which-key)
      (use-package which-key
        :config
        (which-key-mode)
      )

      ;; (themes)
      (use-package doom-themes
        :ensure t
        :config
          (setq 
            doom-themes-enable-bold t
            doom-themes-enable-italic t
          )
          (load-theme 'doom-one t)

        ;; Enable flashing mode-line on errors
          (doom-themes-visual-bell-config)
          (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
          (doom-themes-treemacs-config)
        )

      ;; (general)
      (use-package general
        :after evil
      )

      (general-create-definer leader-bindings
        :keymaps '(normal insert visual emacs)
        :prefix "${leader-key}"
        :global-prefix "C-SPC"
      )

      ;; TODO: I've decided it's generally better if
      ;; I do this stuff with hydras. I should use
      ;; which-key for general help performing a keybinding
      ;; but hydras for defining a global map menu.
      ;; I've taken this decision because apparently not
      ;; all package have a convenient `-command-map` prefix
      ;; that I can use for showing all choices
      (leader-bindings
        ;; TODO: for some reason, this won't work if I assign
        ;; it to the projectile-mode-map. Find out why
        ;; :keymaps 'projectile-mode-map
        "p" 'projectile-command-map
        "${lsp-key}" 'lsp-command-map
        "t" 'treemacs
        "c l" 'flycheck-list-errors
        "c n" 'flyckeck-next-error
        "c p" 'flyckeck-previous-error
      )
    '';
  };
}
