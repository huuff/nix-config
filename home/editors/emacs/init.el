;; TODO: Make all === lines the same length in all files (very important)
;; TODO: Maybe use literate-calc-mode
;; TODO: repl-driven-development might be incredibly cool
;; TODO: Try out lsp-booster, it may be impressive
;; TODO: Maybe I should also shackle helpful same as help
;; TODO: I did rebind k to eldoc-box, but I'd like being able to open eldoc in a separate window split like I did before. I don't even remember what the actual command was, so maybe I should unbind it and try `C-h k k` to find it
;; TODO: Make eshell a popup buffer
;; TODO: Use eglot-x?
;; TODO: Maybe I should use defcustom for my vars instead of defvar
;; TODO: Unify some of my configs (pop-up buffers, without tabs) into a haf/popup-buffers variable, and also add to it cargo-test
;; TODO: Some form of changing font size for all buffers, please
;; TODO: I think any errors during nix-rebuild that say "assignment to free variable" mean that I'm assigning to variables that don't even exist
;; TODO: Use meow? It seems pretty rad
;; TODO: Some evil-mc keybindings for creating a cursor on each line beginning/end
;; TODO: Maybe start using transient instead of hydra?
;; TODO: My rustic window management stuff doesn't work sometimes
;; TODO: Maybe set-up some code folding
;; TODO: A hydra for changing the font size
;; TODO: vterm maybe?
;; TODO: Maybe use tempel instead of yasnippet
;; TODO: Can I make popper.el buffers be "other window"? Otherwise, I can't close them with C-w o!!
;; TODO: Try to use :custom in use-package instead of :config with a setq
;; TODO: There's some error that appears when building it with nix, build with -L to find out what it is
;; TODO: An embark action to toggle mut in rust-mode (and maybe others?) (is there a toggle pub?)
;; TODO: A hydra to interactively indent/deindent visually selected regions without losing the selection
;; TODO: Can I make some packages load lazily with :command? Is it worth it?
;; TODO: I'd love to use project-x, but it's not on MELPA
;; TODO: Use move-text with the advice to indent the region
;; TODO: Maybe I should add #' in front of my functions (that I defined with defun or lambda)? It's supposed to compile them so it should be faster?
;; TODO: Enable the daemon mode
;; TODO: eglot's flymake diagnostics for rust aren't long enough and it drives me crazy!
;; I might get a "mismatched types" but need to run rustc to know which type was the actual
;; and which was the expected!
;; TODO: Maybe check out whether I want some corfu extensions (see https://github.com/minad/corfu#extensions)
;; TODO: Maybe I should use electric-pair-mode instead of smartparens?
;; TODO: follow this config a little https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/ 
;; TODO: Entire buffer textobj would be nice, I do `cae` or `dae` a lot in vim
;; TODO: Comment and explain ALL packages
;; TODO: Reorganize it a little
;; TODO: There are two commands I need to run so fonts work. Is there anyway I could automate it or notify whether it's needed?:
;; - nerd-icons-install-fonts
;; - all-the-icons-install-fonts
;; TODO: Use magit
;; TODO: Set correct dependencies between packages with use-package (:after)
;; TODO: Indent guides for YAML
;; TODO: Since I'm using the nixpkgs overlay, I think there is some binary cache I have to setup
;; TODO: Use fast-scroll?
;; TODO: Should I use the built-in dired and configure it in a cool way so I don't need treemacs? UPDATE: Maybe even add dirvish to it
;; TODO: Use flymake-clippy?
;; TODO: Maybe use hl-todo-mode and consult-todo
;; TODO: Maybe enable go-to-address-mode?
;; TODO: Maybe try embark with which-key integration? There's apparently an elisp snippet somewhere that does this
;; TODO: Use rainbow-mode?
;; TODO: Some way to go back to the previous buffer for when I'm switching between projects
;; TODO: A config to go to "alternate files", such as, for example, going to the test, or the the css module of a file
;; TODO: Maybe set up dictionaries and spell checking?
;; TODO: Use prism, at least for elisp buffers?
;; TODO: Maybe use bufler? It's pretty cool, but I'd love to have a preview (consult) for switching buffers. Also, workspaces seem pretty sensible and I could use them for my tab-line

;; refresh open buffers when filesystem changes
(global-auto-revert-mode)

;; disable ugly top toolbars and scroll bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; maximum highlighting with tree-sitter
(setq treesit-font-lock-level 4)

;; do not use tabs for indenting (only spaces)
(setq-default indent-tabs-mode nil)

;; show line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; set font 
(set-frame-font "Fira Code 10" nil t)

;; (use-package)
(eval-when-compile
  (require 'use-package))

;; must load it early or otherwise use-package's :general
;; won't work. I thought use-package was supposed to fix
;; precisely this issue but whatever
(use-package general
  :init
  ;; automatically unbind any definition that conflicts with mine
  (general-auto-unbind-keys t)
  )


;; allow pasting with Ctrl+V, even in minibuffer
(general-define-key "C-S-v" 'yank)

;; yasnippet
(use-package yasnippet
  :defer 2
  :config
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :defer
  )

(use-package yasnippet-capf
  :after (yasnippet cape)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  )

;; super-save
;; =====================
;; auto-saves the buffer to its file on certain events (such as switching buffer)
;; TODO: This package has a list of hooks on which to save... maybe I could put here saving after an eglot code action rather than use my own hook there.
(use-package super-save
  :ensure t
  :custom
  ;; auto-save also when idle
  (super-save-auto-save-when-idle t)
  :config
  (super-save-mode +1)
  ;; disable default horrible autosave mode that leaves thousands of backup files
  (setq auto-save-default nil))

;; (bind-key) necessary for use-package
(use-package bind-key
  :ensure t
  :config
  (add-to-list 'same-window-buffer-names "*Personal Keybindings*")
  )

;; TODO: I enabled set-navigator but have no navigation links! maybe try choosing some
;; TODO: It'd be huge to display an elfeed with planetemacs
;; TODO: Change the image to the one that appears in the awesome-emacs repo
;; dashboard
;; =====================
;; nice dash board for the first screen
(use-package dashboard
  :ensure t
  :custom
  ;; choose project.el vs projectile for the project list
  (dashboard-projects-backend 'project-el)
  ;; choose widgets, order and size in lines for the dashboard
  ;; for example (projects . 5) shows 5 projects
  (dashboard-items '((projects . 7)
                     (recents  . 5)))
  ;; choose icons between all-the-icons and nerd-icons
  (dashboard-icon-type 'all-the-icons)
  ;; display icons (in general)
  (dashboard-display-icons-p t)
  ;; display icons in widget headings
  (dashboard-set-heading-icons t)
  ;; display file icons
  (dashboard-set-file-icons t)
  ;; show navigator with some options below the banner
  (dashboard-set-navigator t)
  ;; self-descriptive
  (dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook))

;; try
;; =====================
;; allows trying out packages with `try «package»`
(use-package try :commands try)

;; add MELPA so we have access to more packages with `try` or `install-package`
;; note that you'll need to call package-refresh-contents to have access to these packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; corfu
;; =====================
;; autocompletion framework that enhances emacs' native completion system
(use-package corfu
  :init
  (global-corfu-mode)
  (setq
   corfu-auto-delay 0.75
   )
  :general
  (:states '(normal insert)
           :keymaps 'override
           "C-SPC" 'completion-at-point
           )
  :custom
  ;; enable autocompletion
  (corfu-auto t)
  )

;; show a pop-up with documentation on each autocompletion candidate after a small delay
(use-package corfu-popupinfo
  :ensure nil ;; already included in corfu
  :after corfu
  :config
  (corfu-popupinfo-mode)
  )

;; orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  )

;; eldoc
(use-package eldoc
  :init
  (setq 
   eldoc-idle-delay 0.75
   )
  )

(use-package eldoc-box
  :general
  (:states 'normal
           :keymaps 'override
           ;; same key as for vim
           "K" 'eldoc-box-help-at-point
           )
  :config
  (setq eldoc-box-clear-with-C-g t)
  )

;; marginalia
(use-package marginalia
  :init
  (marginalia-mode)
  )

;; helpful
;; =====================
;; much better help mode that:
;; * includes source code for elisp functions
;; * prettifies docs
;; * includes more links to other definitions
;; * includes symbol properties
;; * has easy links to disassemble compiled values or debug functions
(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-command]  . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key]      . helpful-key)
         ([remap describe-symbol]   . helpful-symbol)
         :map emacs-lisp-mode-map
         ("C-c C-d"                 . helpful-at-point)
         :map lisp-interaction-mode-map
         ("C-c C-d"                 . helpful-at-point)))

;; consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (
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
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
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
  (defun haf/defer/treemacs ()
    (haf/delay-fun (lambda () (save-selected-window (treemacs)))))
  :hook (emacs-startup . haf/defer/treemacs)

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
  ;; enable redo
  (evil-set-undo-system 'undo-redo)
  )

;; evil-collection
;; some features:
;; * vim-unimpaired keybindings
;; * hungry-delete integration with evil
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )

(use-package evil-nerd-commenter
  :init
  (evilnc-default-hotkeys)
  )
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
  ;; TODO: Use embark for this!
  (:states '(normal insert)
           "C-c +" 'evil-numbers/inc-at-pt
           "C-c -" 'evil-numbers/dec-at-pt
           )
  )

;; TODO: Can I configure it so n and N also cycle results as for search?
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
  ;; TODO: I don't even use any of these... bind them to something else?
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

;; TODO: There's an alternative package for this that does the same but it's not specifically for evil, right? what was its name? maybe ask chatgpt. UPDATE: It's volatile-highlights
;; TODO: Colors aren't very visible. This issue:
;; https://github.com/edkolev/evil-goggles/issues/33
;; says it's because doom-themes but I've tried changing
;; it with no result
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode))

;; hungry-delete
;; deletes large sequences of whitespace with a single press
(use-package hungry-delete
  :config
  (setq 
   ;; do not delete aggresively join words
   ;; without it, deleting the space between "word1      word2"
   ;; ends up with "word1word2", with it, it's "word1 word2"
   hungry-delete-join-reluctantly t

   ;; I'm not even entirely sure what these characters are
   ;; but I copied this off https://github.com/nflath/hungry-delete/issues/20
   ;; so far, it prevents hungry-delete from being *too* hungry and at least doesn't delete newlines
   hungry-delete-chars-to-skip " \t\r\f\v"
   )
  (global-hungry-delete-mode)
  )

;; expand-region
;; TODO: Maybe I should use combobulate when I can configure tree-sitter?
(use-package expand-region
  :config
  ;; disable fast keys since my hydra does that and they conflict otherwise
  (setq expand-region-fast-keys-enabled nil)
  )

;; lorem-ipsum
;; TODO: Maybe also configure separators, etc. for text-mode and markdown-mode
;; TODO: Can I configure an abbrev for this? (so writing lorem would expand to a paragraph) That's be cool
(use-package lorem-ipsum
  :commands (lorem-ipsum-insert-sentences lorem-ipsum-insert-list lorem-ipsum-insert-paragraphs)
  :preface 
  (defun haf/configure-html-lorem-ipsum ()
    (setq lorem-ipsum-paragraph-separator "<br><br>\n"
          lorem-ipsum-sentence-separator "&nbsp;&nbsp;"
          lorem-ipsum-list-beginning "<ul>\n"
          lorem-ipsum-list-bullet "<li>"
          lorem-ipsum-list-item-end "</li>\n"
          lorem-ipsum-list-end "</ul>\n"))

  :hook ((web-mode . haf/configure-html-lorem-ipsum))
  )

;; eglot
;; ================
;; Built-in integration with the LSP protocol
;; TODO: Enable it for nix
;; TODO: Maybe enable it for rust? rustic mode does it but I don't know for how long I'll use it
(use-package eglot
  :custom
  (eglot-ignored-capabilities
   '(
     ;; disable semantic highlighting, leave it for tree-sitter
     :semanticTokensProvider
     )
   )
  :config 
  ;; auto-save current buffer when any code action is executed.
  ;; it helps mainly with rust, since rust-analyzer only lints on save. 
  ;; this makes the warnings in the buffer obsolete, and it's especially
  ;; bothersome when I'm cycling through errors.
  ;; TODO: Maybe only set it for rustic-mode?
  (advice-add 'eglot-code-actions :after #'(lambda (&rest r) (save-buffer)))

  (add-to-list 'eglot-server-programs
               '(svelte-mode . ("svelteserver" "--stdio")))

  :hook
  ((tsx-ts-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (svelte-mode . eglot-ensure))
  )

;; apheleia
;; =====================
;; auto-format buffers on save
(use-package apheleia
  :config (apheleia-global-mode +1))

;; dape
;; =====================
;; Adapters for the DAP protocol to debug in emacs
;; TODO: Maybe use a transient/hydra? As in: https://github.com/svaante/dape/issues/6
(use-package dape
  :config
  (add-to-list 'dape-configs
               ;; XXX: custom (mostly copy-pasted) config for rust so I can
               ;; inject (substitute) codelldb from nix
               `(codelldb-rust 
                 modes (rust-mode rust-ts-mode)
                 command "@codelldb@" 
                 :type "lldb" 
                 :request "launch" 
                 command-args ("--port" :autoport "--settings" "{\"sourceLanguages\":[\"rust\"]}")
                 ensure dape-ensure-command port :autoport fn dape-config-autoport 
                 :cwd dape-cwd-fn 
                 :program dape-find-file 
                 :args [])
               )
  )

;; cape
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  )

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  )

;; save history over emacs restarts, useful for vertico which sorts by it
(use-package savehist
  :ensure nil ;; already included in emacs
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
;; TODO (MAYBE) rustic-mode won't work with tree-sitter, so maybe I'll just have to drop it
(use-package rustic
  :init
  (setq rustic-lsp-client 'eglot)
                                        ;:config
  (advice-add 'rustic-cargo-add :before #'haf/advice-set-ran-rustic-dependency-management)
  (advice-add 'rustic-cargo-rm :before #'haf/advice-set-ran-rustic-dependency-management)
  )
(use-package rust-mode)

;;FUTURE: This may not be needed in emacs 30 or further,
;;but currently, it's much easier this way
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; remove rust because rustic-mode is not compatible with treesitter
  (delete 'rust treesit-auto-langs)

  ;; set v0.20.2 version for typescript since the default is master and that's
  ;; incompatible with emacs' version
  (let ((typescript-recipe (make-treesit-auto-recipe
                            :lang 'typescript
                            :ts-mode 'typescript-ts-mode
                            :remap 'typescript-mode
                            :requires 'tsx
                            :url "https://github.com/tree-sitter/tree-sitter-typescript"
                            :revision "v0.20.2"
                            :source-dir "typescript/src"
                            :ext "\\.ts\\'")))
    (add-to-list 'treesit-auto-recipe-list typescript-recipe))

  ;; auto switch to the treesitter mode for all langs included in treesit-auto-langs
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  )

;; shackle
;; allows configuring in what way each buffer will show
(use-package shackle
  :init
  (shackle-mode)
  :config
  (setq
   shackle-rules '(
                   (help-mode :size 0.3 :align below :select t)

                   ('(:custom haf/is-rustic-dependency-management) :size 0.3 :align below)
                   (rustic-compilation-mode :size 0.4 :align right :select t)
                   (rustic-cargo-clippy-mode :size 0.4 :align right :select t)
                   )
   )
  )

;; popper
;; mark buffers as "pop-up", making them easy to dismiss, toggle and cycle through
(use-package popper
  :ensure t
  :init
  (setq popper-reference-buffers '(
                                   "\\*Messages\\*"
                                   "Output\\*$"
                                   "\\*Async Shell Command\\*"
                                   help-mode
                                   compilation-mode

                                   ;; rust
                                   rustic-compilation-mode
                                   rustic-cargo-clippy-mode
                                   )
        )
  (popper-mode +1)
  ;; enables echo area hints
  (popper-echo-mode +1)
  ;; do not do any display control, let shackle do it
  (setq popper-display-control nil)
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

(defun haf/project-remember-current-project ()
  "Remembers the current project"
  (interactive)
  (project-remember-project (project-current))
  ;; TODO: I could just do this with a simple substitution like (message "remembering project '%S'") or smth
  (message (concat "Remembering project '" (caddr (project-current)) "'"))
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
(defun haf/switch-theme-by-mode (&optional args)
  "Switches theme depending on current major-mode"
  (interactive)
  (let ((next-theme (cl-case major-mode
                      ('rustic-mode 'doom-gruvbox)
                      ('emacs-lisp-mode 'doom-one)
                      ('typescript-ts-mode 'doom-material)
                      ('svelte-mode 'doom-moonlight)
                      (t nil)
                      )))
    (when next-theme (progn 
                       (dolist (theme custom-enabled-themes) (disable-theme theme))
                       (load-theme next-theme t)))))


(use-package doom-themes
  :ensure t
  :config
  (setq 
   doom-themes-enable-bold t
   doom-themes-enable-italic t
   )
  (load-theme 'doom-one t)

  ;; enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; setup treemacs
  (setq doom-themes-treemacs-theme "doom-colors") 
  (doom-themes-treemacs-config)

  ;; set theme by mode after a small delay so the mode
  ;; is initialized
  (haf/delay-fun #'haf/switch-theme-by-mode 2)

  ;; switch theme depending on language when the window changes, note that it only works here, and not in use-package's :hook
  ;; TODO: This doesn't work when changing tabs with tab-line!
  (add-hook 'window-selection-change-functions #'haf/switch-theme-by-mode)
  )

;; solaire-mode
;; =====================
;; gives a brighter color to file-visiting buffers (where editing happens)
;; and a darker one to the rest, so the "main" buffer is highlighted
;; TODO: Pretty cool... but it's not picking up treemacs?
(use-package solaire-mode
  :config
  (solaire-global-mode +1))

;; modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  )

;; TODO: The pulse functions config doesn't even work for me and I use hooks...
;; maybe I should just use pulse and manually configure it myself
;; pulsar
;; =====================
;; highlights the current line under some conditions (such as window changes)
;; so you don't lose your cursor
(use-package pulsar
  :custom
  ;; enable pulsing (but it should be enabled by default anyway)
  (pulsar-pulse t)
  ;; prettier color
  (pulsar-face 'pulsar-magenta)
  ;; I set this to yellow because it's ugly and I don't even know when this should appear, so maybe if it's ugly I'll notice when it's firing
  (pulsar-highlight-face 'pulsar-yellow)
  :preface
  (defun haf/pulsar-pulse-line (_) 
    "Pulses line but also takes an argument, so I can use it as a window-selection-change-functions"
    (pulsar-pulse-line))
  :config
  ;; integration with consult
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-center)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
  ;; pulse on next-error
  (add-hook 'next-error-hook #'pulsar-pulse-line)
  ;; pulse on window changes
  (add-hook 'window-selection-change-functions #'haf/pulsar-pulse-line)
  (add-hook 'window-buffer-change-functions #'haf/pulsar-pulse-line)
  ;; enable it globaly
  (pulsar-global-mode 1))


;; direnv
(use-package direnv
  :config
  (direnv-mode))

;; web-mode
(use-package web-mode
  :config 
  ;; define a new mode for svelte so I can hook to it specifically instead of
  ;; every web-mode
  (define-derived-mode svelte-mode web-mode "Svelte")
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode)))

;; keybindings
;; =====================
;; keybindings that are supposed to work in normal state, but that
;; I don't expect (or need) to work in insert state
(general-create-definer normal-leader-bindings
  :states '(normal visual emacs)
  :prefix "SPC"
  )

;; TODO: Some treemacs keybindings, maybe for setting up workspaces and stuff
(normal-leader-bindings
  "t" 'treemacs
  "e" '(hydra-flymake/body :which-key "Errors")
  ;; TODO: Can I nest a prefix?
  ;; TODO: If I could, it'd be great to add a which-key hint there, currently l just shows "+prefix"
  ;; TODO: for some of these (such as go to definition and go to implementation), a target is required (a workspace symbol). Wouldn't they be better as embark actions? UPDATE: I'm sure they exist as embark actions, but maybe I should fix keybindings
  ;; so they match these? such as embark-act + d for go to definition

  ;; code (mostly LSP, but also xref)
  "l a" '(eglot-code-actions :which-key "Code actions")
  "l r" '(eglot-rename :which-key "Rename")
  "l f" '(eglot-format-buffer :which-key "Format buffer")
  ;; TODO: Maybe use xref-find-definitions?
  "l d" '(eglot-find-declaration :which-key "Go to definition")
  "l o" '(eglot-code-action-organize-imports :which-key "Organize imports")
  "l i" '(eglot-find-implementation :which-key "Go to implementation")
  "l u" '(xref-find-references :which-key "Find usages")
  "l m" '(consult-imenu :which-key "imenu")

  ;; TODO: compile for non-rustic mode
  "p f" '(consult-fd :which-key "Find file")
  "p F" '(consult-ripgrep :which-key "Find text")
  "p r" '(haf/project-remember-current-project :which-key "Remember project")
  "p p" '(project-switch-project :which-key "Switch project")

  "w w" '(popper-toggle :which-key "Toggle popup")
  "w t" '(popper-cycle :which-key "Cycle popup")
  "w o" '(popper-toggle-type :which-key "Change popup type")
  )

(normal-leader-bindings 
  :keymaps 'rustic-mode-map
  ;; TODO: Maybe cargo-test DWIM? Seems cool. Or do that with embark?
  "p t" '(rustic-cargo-test :which-key "Run tests")
  "p c" '(rustic-cargo-build :which-key "Compile")
  "p k" '(rustic-cargo-clippy :which-key "Clippy")
  )

;; keybindings that are supposed to work in all states (included insert)
(general-create-definer insert-leader-bindings
  :states '(normal insert visual emacs)
  :keymaps 'override
  :prefix "C-z"
  )

(insert-leader-bindings
  "TAB f" '(cape-file :which-key "File")
  "TAB TAB" '(completion-at-point :which-key "Normal")
  "TAB w" '(cape-dict :which-key "Dictionary")
  "TAB :" '(cape-emoji :which-key "Emoji")
  "TAB \\" '(cape-tex :which-key "Tex")
  "TAB _" '(cape-tex :which-key "Tex")
  "TAB ^" '(cape-tex :which-key "Tex")
  "TAB &" '(cape-tex :which-key "SGML")
  "TAB s" '(yasnippet-capf :which-key "Snippet")

  "C-w" '(haf/expand-and-start-region-hydra :which-key "Expand region")
  "C-d" '(haf/next-cursor-and-start-region-hydra :which-key "Add cursor")

  "t k" '(haf/tab-line-close-other-tabs :which-key "Kill other tabs")
  )
