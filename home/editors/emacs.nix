{ pkgs, emacs-overlay, ... }:

# TASKS:
# TODO: Some keybindings like intellij iDEA like:
  # * shift + F6: Rename
  # * shift + shift: Find file
# TODO: Seems like lsp-ui lens diagnostics might break a lot of stuff when moving!
# TODO: LSP code actions don't seem to actually be using helm
# TODO: Some cool way of showing available code actions
# TODO: Some keybindings to cycle through flycheck list diagnostics
# TODO: Some more evil packages like:
  # * evil-args
# TODO: Comment and explain ALL packages
# TODO: Reorganize it a little
# TODO: Try to have all packages in use-package
# TODO: Use emacs-overlay feature to install packages from use-package?
# TODO: Auto install icons
# TODO: configure Helm for more features (currently it's only for M-x)
# TODO: Use magit
# TODO: Search MELPA for any packages that contain the names of any packages I use, see if there are any more integrations I'm missing!
# TODO: Set correct dependencies between packages with use-package (:after)
# TODO: Maybe some nice status line?
# TODO: DAP mode for debugging
# TODO: Use expand-region
# TODO: Multi-cursor stuff
# TODO: A mouse hover pop-up for flycheck would be nice
# TODO: Indent guides for YAML
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

      # vim emulation
      epkgs.evil
      epkgs.evil-nerd-commenter
      epkgs.evil-surround
      epkgs.evil-matchit
      epkgs.evil-numbers
      epkgs.evil-goggles
      epkgs.evil-snipe
      epkgs.evil-collection

      # yasnippet
      epkgs.yasnippet
      epkgs.yasnippet-snippets

      # needed for advanced features of treemacs for git
      pkgs.python3
    ];

    extraConfig = ''
      ;; variable set up
      (defconst my-leader "SPC")
      (defconst lsp-key "l")

      ;; must load it early or otherwise use-package's :general
      ;; won't work. I thought use-package was supposed to fix
      ;; precisely this issue but whatever
      (use-package general)

      ;; disable ugly top toolbars and scroll bars
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (menu-bar-mode -1)

      ;; show line numbers in programming modes
      (add-hook 'prog-mode-hook 'display-line-numbers-mode)

      ;; set font 
      (set-frame-font "Fira Code 10" nil t)

      ;; allow pasting with Ctrl+V, even in minibuffer
      ;; TODO: Maybe use general for this
      (global-set-key (kbd "C-v") 'yank)

      ;; yasnippet
      (use-package yasnippet
        :defer 2
        :config
        (yas-global-mode 1)
      )

      (use-package yasnippet-snippets
        :defer
      )

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
      (use-package company
        :ensure t
        :config
          (setq
            ;; recommended settings by lsp-mode
            ;; https://emacs-lsp.github.io/lsp-mode/page/main-features/#completion-at-point
            company-minimum-prefix-length 1
            company-idle-delay 0.01
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
          (lsp-mode . lsp-inlay-hints-mode)
        )
        :config
          ;; don't know why but only these two commands
          ;; will make lsp work with leader key and general.el
          (setq lsp-keymap-prefix (concat my-leader " " lsp-key))
          (fset 'lsp-command-map lsp-command-map)
          (setq lsp-inlay-hint-enable t)
        :commands lsp
      )
      (use-package lsp-ui :commands lsp-ui-mode)
      (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
      (use-package helm-lsp :commands helm-lsp-workspace-symbol)


      ;; (treemacs)
      (use-package treemacs
        :ensure t
        :defer t

        ;; start treemacs on startup (with projectile)
        ;; however, it's prone to open an empty *scratch* buffer
        ;; when doing this, which is infuriating.
        ;; delaying it is the only solution I found
        ;; https://github.com/Alexander-Miller/treemacs/issues/258#issuecomment-831489403
        ;; the save-selected-window prevents it from being focused when opened
        :preface
        (defun defer/treemacs ()
          (run-with-idle-timer 1 nil (lambda () (save-selected-window (treemacs)))))
        :hook (projectile-mode . defer/treemacs)

        :config
        ;; always select the current file in treemacs
        (treemacs-follow-mode t)
      )
      (use-package treemacs-evil
        :after (treemacs evil)
      )


      ;; (all-the-icons) 
      (use-package all-the-icons
        :if (display-graphic-p))

      ;; (evil)
      (use-package evil
        :init
        ;; these 2 are necessary for evil-collection
        (setq evil-want-integration t)
        (setq evil-want-keybinding nil)
        :config
        (evil-mode 1)
      )

      ;; I mostly use it because it includes vim-unimpaired
      ;; keybindings
      (use-package evil-collection
        :after evil
        :ensure t
        :config
        (evil-collection-init)
      )

      ;; TODO: Put its keybindings here with :general
      (use-package evil-nerd-commenter)
      (use-package evil-surround
        :after evil
        :config
        (global-evil-surround-mode 1)
      )

      (use-package evil-matchit
        :after evil
        :config
        (global-evil-matchit-mode 1)
      )

      (use-package evil-numbers
        :general
          (:states '(normal insert)
            "C-c +" 'evil-numbers/inc-at-pt
            "C-c -" 'evil-numbers/dec-at-pt
          )
      )

      (use-package evil-snipe
        :after evil
        :config
        (evil-snipe-mode +1)
        (evil-snipe-override-mode +1)
        ;; snipe in whole buffer, not just current line
        (setq evil-snipe-scope 'whole-buffer)
      )

      ;; TODO: Colors aren't very visible. This issue:
      ;; https://github.com/edkolev/evil-goggles/issues/33
      ;; says it's because doom-themes but I've tried changing
      ;; it with no result
      (use-package evil-goggles
        :ensure t
        :config
        (evil-goggles-mode)
      )

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
        :hook 
        ('tree-sitter-after-on . tree-sitter-hl-mode)
      )

      ;; (centaur-tabs)
      (use-package centaur-tabs
        :init
          (setq centaur-tabs-enable-key-bindings t)
        :demand
        :config
          (centaur-tabs-mode t)
          ;; make tabs take full width
          (centaur-tabs-headline-match)
          ;; show icons
          (setq centaur-tabs-set-icons t)
          ;; gray-out unactive tab
          (setq centaur-tabs-gray-out-icons 'buffer)
          ;; left active tab indicator
          (setq centaur-tabs-set-bar 'left)
          ;; show whether tab is modified
          (setq centaur-tabs-set-modified-marker t)
          ;; do not allow crossing tab groups by changing tab 
          ;; (prevents going to weird hidden buffers when going
          ;; to next tab on the last one)
          (setq centaur-tabs-cycle-scope 'tabs)
        :bind
        ;; vim-like change tabg with `g t` and `g T`
          (:map evil-normal-state-map
            ("g t" . centaur-tabs-forward)
            ("g T" . centaur-tabs-backward))
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

          (push "*scratch*" popwin:special-display-config)
      )

      ;; (smartparens)
      (use-package smartparens
        :defer t
        :hook (prog-mode . smartparens-mode)
        :config
          ;; load default config
          (require 'smartparens-config)
      )

      ;; (projectile)
      (use-package projectile
        :config
        (projectile-mode +1)
      )
      (use-package treemacs-projectile
        :after (treemacs projectile)
      )
      (use-package helm-projectile
        :config
        (helm-projectile-on)
      )

      ;; (which-key)
      (use-package which-key
        :init
        ;; both of these lines enable compatibility with evil
        (setq which-key-allow-evil-operators t)
        (setq which-key-show-operator-state-maps t)
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
  
      (general-create-definer leader-bindings
        :keymaps '(normal insert visual emacs)
        :prefix my-leader
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
        lsp-key 'lsp-command-map
        "t" 'treemacs
        ;; TODO: Use lsp-treemacs-errors-list
        "e l" 'flycheck-list-errors
        "e n" 'flyckeck-next-error
        "e p" 'flyckeck-previous-error
        ;; TODO: Maybe I'm missing some keybinding for evilnc-comment-operator (for textobjx)
        ;; TODO: Maybe instead of doing this for some leader prefix config, why not do this under some other 
        ;; generally-useful prefix that I can use in insert or normal mode?
        ;; for example, I've put evil-numbers under C-c, and I could put this under the same prefix
        "c SPC" 'evilnc-comment-or-uncomment-lines
      )
    '';
  };
}
