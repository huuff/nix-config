;; TODO: follow this config a little https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/ 
;; TODO: Use the holy grail of emacs: Vertico+Consult+Orderless+Embark+Marginalia+Corfu (Vertico+Corfu+Marginalia+Embark+Consult done!)
;; TODO: Set auto-save for rust so the LSP works
;; TODO: Entire buffer textobj would be nice, I do `cae` or `dae` a lot in vim
;; TODO: LSP code actions don't seem to actually be using helm
;; TODO: Some cool way of showing available code actions
;; TODO: Some keybindings to cycle through flycheck list diagnostics. UPDATE: Or maybe use lsp-treemacs-errors-list
;; TODO: Comment and explain ALL packages
;; TODO: Reorganize it a little
;; TODO: There are two commands I need to run so fonts work. Is there anyway I could automate it or notify whether it's needed?:
  ;; - nerd-icons-install-fonts
  ;; - all-the-icons-install-fonts
;; TODO: Use magit
;; TODO: Search MELPA for any packages that contain the names of any packages I use, see if there are any more integrations I'm missing!
;; TODO: Set correct dependencies between packages with use-package (:after)
;; TODO: DAP mode for debugging
;; TODO: Use expand-region
;; TODO: Multi-cursor stuff
;; TODO: Indent guides for YAML
;; TODO: Keybinding to close all other tabs with centaur
;; TODO: Maybe I should use popper.el instead of popwin.el
;; TODO: I don't think tree-sitter-mode is even working... emacs starts out with no highlighting and only appears when I disable and re-enable tree-sitter-hl-mode
;; TODO: Since I'm using the nixpkgs overlay, I think there is some binary cache I have to setup
;; TODO: Can't I bind C-SPC to autocomplete?

;; variable set up
(defconst my-leader "SPC")

;; refresh open buffers when filesystem changes
(global-auto-revert-mode)

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

;; corfu
(use-package corfu
  :init
  (global-corfu-mode)
  (setq
    corfu-auto-delay 0.75
  )
  :custom
    ;; enable autocompletion
    (corfu-auto t)
)

;; eldoc
(use-package eldoc
  :init
  (setq 
    eldoc-idle-delay 0.75
  )
)

;; marginalia
(use-package marginalia
  :init
  (marginalia-mode)
)

;; consult
;; TODO: Install and use ripgrep (rg)
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
)

;; embark
(use-package embark
  :ensure t
  ;; TODO: Can't I bind C-.? It conflicts with evil-repeat
  :general
  ("C-," 'embark-act)
)

;; consult integration
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
)

;; (nix-mode)
(use-package nix-mode
  :mode "\\.nix\\'"
)

;; (treemacs)
(use-package treemacs
  :ensure t
  :defer t

  ;; start treemacs on startup
  ;; however, it's prone to open an empty *scratch* buffer
  ;; when doing this, which is infuriating.
  ;; delaying it is the only solution I found
  ;; https://github.com/Alexander-Miller/treemacs/issues/258#issuecomment-831489403
  ;; the save-selected-window prevents it from being focused when opened
  :preface
  (defun defer/treemacs ()
    (run-with-idle-timer 1 nil (lambda () (save-selected-window (treemacs)))))
  :hook (emacs-startup . defer/treemacs)

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

;; TODO: Try to set up some way to exchange args order
;; for example see: https://github.com/wcsmith/evil-args/issues/4
(use-package evil-args
  :after evil
  :general
  (evil-inner-text-objects-map "a" 'evil-inner-arg)
  (evil-outer-text-objects-map "a" 'evil-outer-arg)
  (:states 'normal
    "L" 'evil-forward-arg
    "H" 'evil-backward-arg
    "K" 'evil-jump-out-args
  )
  (:states 'motion
    "L" 'evil-forward-arg
    "H" 'evil-backward-arg
  )
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

;; eglot
;; TODO: Enable it for nix
(use-package eglot
  :custom
  (eglot-ignored-capabilities
    '(
      ;; disable semantic highlighting, leave it for tree-sitter
      :semanticTokensProvider
    )
  )
)

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
)

;; save history over emacs restarts, useful for vertico which sorts by it
;; TODO: Not working? nix screams when building
(use-package savehist
  :init
  (savehist-mode)
)

;; flymake
(use-package flymake
  :init
  (setq
    flymake-no-changes-timeout 0.5
  )
)

;; sideline
(use-package sideline
  :init 
  (setq
    sideline-display-backend-name t
    sideline-backends-right '(sideline-flymake)
  )
  :hook (
    (flymake-mode  . sideline-mode)
  )
)
(use-package sideline-flymake)

;; rust
(use-package rustic
  :init
  (setq rustic-lsp-client 'eglot)
)
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

;; (project)

(use-package project
  :init
  (setq 
    ;; use directories with these files as project roots (useful so it detects nested projects)
    project-vc-extra-root-markers '("Cargo.toml")
    ;; ignore these directories (normally it gets them from .gitignore, but this is useful for nested projects)
    project-vc-ignores '("target/" "bin/" "obj/")
  )
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
    (setq doom-themes-treemacs-theme "doom-colors") 
    (doom-themes-treemacs-config)
)

;; modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
)

;; keybindings

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
  "t" 'treemacs
  ;; TODO: Maybe I'm missing some keybinding for evilnc-comment-operator (for textobjx)
  ;; TODO: Maybe instead of doing this for some leader prefix config, why not do this under some other 
  ;; generally-useful prefix that I can use in insert or normal mode?
  ;; for example, I've put evil-numbers under C-c, and I could put this under the same prefix
  "c SPC" 'evilnc-comment-or-uncomment-lines
)
