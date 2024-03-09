;; TODO: A keybinding for opening magit
;; TODO: Remove tabs from *dape-* buffers
;; TODO: Can I make magit close the status buffer automatically after a push?
;; TODO: Use this cool snippet for maximizing windows https://www.reddit.com/r/emacs/comments/yzjmmf/comment/ix1xpab/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
;; TODO: Maybe use prodigy
;; TODO: Maybe use golden-ratio
;; TODO: Perhaps I could use aggressive-indent-mode for elisp only
;; TODO: Maybe I should bring back evil-collection for dired and try to get used to wdired instead
;; TODO: Use justl mode
;; TODO: persistent-scratch-mode might be cool
;; TODO: Install eglot-signature-eldoc-talkative
;; TODO: Maybe use evil-snipe only for the current line and avy for all else
;; TODO: Maybe use evil-quickscope
;; TODO: Maybe use no-littering?
;; TODO: Use anzu? Not excessively important but might improve the experience
;; TODO: Maybe use literate-calc-mode
;; TODO: repl-driven-development might be incredibly cool
;; TODO: Try out lsp-booster, it may be impressive. UPDATE: Also add these configs https://www.reddit.com/r/emacs/comments/1aw6xkc/comment/kriu3ye/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
;; TODO: I did rebind k to eldoc-box, but I'd like being able to open eldoc in a separate window split like I did before. I don't even remember what the actual command was, so maybe I should unbind it and try `C-h k k` to find it
;; TODO: Use eglot-x?
;; TODO: Maybe I should use defcustom for my vars instead of defvar
;; TODO: Some form of changing font size for all buffers, please
;; TODO: I think any errors during nix-rebuild that say "assignment to free variable" mean that I'm assigning to variables that don't even exist
;; TODO: Use meow? It seems pretty rad
;; TODO: Some evil-mc keybindings for creating a cursor on each line beginning/end
;; TODO: Maybe start using transient instead of hydra?
;; TODO: Maybe set-up some code folding. UPDATE: Theres a cool ts-fold package that does folding with treesitter, but I don't think it works with the builtin treesitter so I may need to wait for a next release
;; TODO: A hydra for changing the font size
;; TODO: Maybe use tempel instead of yasnippet
;; TODO: Can I make popper.el buffers be "other window"? Otherwise, I can't close them with C-w o!!
;; TODO: Try to use :custom in use-package instead of :config with a setq
;; TODO: There's some error that appears when building it with nix, build with -L to find out what it is
;; TODO: A hydra to interactively indent/deindent visually selected regions without losing the selection
;; TODO: Can I make some packages load lazily with :command? Is it worth it?
;; TODO: I'd love to use project-x, but it's not on MELPA
;; TODO: Use move-text with the advice to indent the region
;; TODO: Maybe I should add #' in front of my functions (that I defined with defun or lambda)? It's supposed to compile them so it should be faster?
;; TODO: Enable the daemon mode
;; TODO: Maybe check out whether I want some corfu extensions (see https://github.com/minad/corfu#extensions)
;; TODO: Maybe I should use electric-pair-mode instead of smartparens?
;; TODO: follow this config a little https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/ 
;; TODO: Entire buffer textobj would be nice, I do `cae` or `dae` a lot in vim
;; TODO: Try to split some sections to different files
;; TODO: There are two commands I need to run so fonts work. Is there anyway I could automate it or notify whether it's needed?:
;; - nerd-icons-install-fonts
;; - all-the-icons-install-fonts
;; TODO: Set correct dependencies between packages with use-package (:after)
;; TODO: Indent guides for YAML and python (https://github.com/jdtsmith/indent-bars)
;; TODO: Since I'm using the nixpkgs overlay, I think there is some binary cache I have to setup
;; TODO: Use flymake-clippy?
;; TODO: Maybe enable go-to-address-mode?
;; TODO: Maybe try embark with which-key integration? There's apparently an elisp snippet somewhere that does this
;; TODO: Some way to go back to the previous buffer for when I'm switching between projects
;; TODO: A config to go to "alternate files", such as, for example, going to the test, or the the css module of a file
;; TODO: Maybe set up dictionaries and spell checking?
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

;; use-package
;; =====================
;; modular package configuration
(eval-when-compile
  (require 'use-package))

;; general
;; =====================
;; keybinding configurations that are evil-aware and simple.
;; I mostly use it with use-package's :general
;; XXX: must load it early or otherwise use-package's :general
;; won't work. I thought use-package was supposed to fix
;; precisely this issue but whatever
(use-package general
  :init
  ;; automatically unbind any definition that conflicts with mine
  (general-auto-unbind-keys t))


;; allow pasting with Ctrl+V, even in minibuffer
(general-define-key "C-S-v" 'yank)

;; yasnippet
;; =====================
;; allows inserting snippets/templates into your buffers
(use-package yasnippet
  :defer 2
  :config
  (yas-global-mode 1))

;; a collection of pre-made snippets
(use-package yasnippet-snippets
  :defer)

;; adds a completion function and integrates it with corfu
(use-package yasnippet-capf
  :after (yasnippet cape)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

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

;; bind-key
;; =====================
;; use-package needs it for its :bind configuration
(use-package bind-key
  :ensure t
  :config
  (add-to-list 'same-window-buffer-names "*Personal Keybindings*"))

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
  (setq corfu-auto-delay 0.75)
  :general
  (:states '(normal insert)
           :keymaps 'override
           "C-SPC" 'completion-at-point)
  :custom
  ;; enable autocompletion
  (corfu-auto t))

;; show a pop-up with documentation on each autocompletion candidate after a small delay
(use-package corfu-popupinfo
  :ensure nil ;; already included in corfu
  :after corfu
  :config
  (corfu-popupinfo-mode))

;; add icons to corfu candidates
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; orderless
;; =====================
;; more completion matching styles for the minibuffer
;; by default, the orderless style allows searching different
;; terms by separating them with spaces
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  )

;; eldoc
;; =====================
;; buil-in documentation browsing mode
(use-package eldoc
  :ensure nil ;; already included in emacs
  :init
  (setq 
   eldoc-idle-delay 0.75))

;; displays eldoc in a floating childframe
(use-package eldoc-box
  :general
  (:states 'normal
           :keymaps 'override
           ;; same key as for vim
           "K" 'eldoc-box-help-at-point)
  :config
  (setq eldoc-box-clear-with-C-g t))

;; marginalia
;; =====================
;; adds pretty nice info to the margin of each option in minibuffer completions
(use-package marginalia
  :init
  (marginalia-mode))

;; nerd-icons-completion
;; =====================
;; adds icons to completion candidates, must do something
;; related to marginalia because it requires special config
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

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

;; rainbow-delimiters
;; =====================
;; highlights parentheses according to nesting level
;; it's pretty useful for lisp-based languages
(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;; consult
;; =====================
;; an assortment of commands that enhance native ones with nicer displays, previews
;; and more intelligent matching
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
  (setq consult-narrow-key "<"))

;; embark
;; =====================
;; gives actions for the thing at which the cursor is
;; it's especially useful for running actions on minibuffer candidates
;; for example, you might use "C-x", find a command and use embar to show its help
;; rather than using "C-h x"
;; so it prevents you from having to learn more keybindings and improves discoverability
(use-package embark
  :ensure t
  :general
  ("C-," 'embark-act))

;; consult integration
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; TODO: I use diffhl to highlight vc-changed files, but this doesn't look that cool and
;; doesn't integrate that well with dired-subtree.. maybe I should use dired-rainbow to configure that?
;; TODO: Is there any way I can manage dired to sort sveltekit files on top? They start with
;; + which I assume is so they appear on top for listings, but for some reason, dired uses `ls`
;; and it just won't show them on top
;; dired
;; =====================
;; the built-in file-manager
(use-package dired
  :ensure nil ;; already included in emacs
  ;; TODO: Maybe this cold be a little bit brighter?
  ;; mark with the whole current line in dired
  ;; TODO: This gets lost after some seconds... can I fix that?
  :hook (dired-mode . hl-line-mode)
  :custom
  ;; sort so directories are on top
  (dired-listing-switches "-aBhl  --group-directories-first")
  :general
  (:keymaps '(dired-mode-map)
            :states '(normal)
            ;; TODO: It'd be cool to have a dired-toggle-mark command that just toggles
            ;; rather than having two separate keys
            "m" 'dired-mark
            "u" 'dired-unmark
            "d" 'dired-do-delete
            "r" 'dired-do-rename
            "!" 'dired-do-shell-command
            "+" 'dired-create-empty-file
            "*" 'dired-create-directory
            "g r" 'revert-buffer))

;; dired-ranger
;; =====================
;; a handful of commands for dired that add functionalities like those of ranger. 
;; for example `dired-ranger-copy` `dired-ranger-mode` and `dired-ranger-paste` allow selecting
;; several files, moving to a directory and then moving/pasting them there
(use-package dired-ranger
  :after dired
  :general
  (:keymaps '(dired-mode-map)
            :states '(normal)
            "c" 'dired-ranger-copy
            "p" 'dired-ranger-paste
            "P" 'dired-ranger-move))


;; TODO: Auto open sidebar when changing a project with project.el
;; TODO: Configure following file (dired-sidebar-should-follow-file and others)
;; TODO: Infuriatingly, forward-history for dired-rename gives the wrong path instead of the current file's path
;; dired-sidebar
;; =====================
;; use dired as a tree directory explorer in a sidebar just like a real IDE
(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (add-to-list 'dired-sidebar-special-refresh-commands 'dired-ranger-move)
  (add-to-list 'dired-sidebar-special-refresh-commands 'dired-ranger-paste)
  (add-to-list 'dired-sidebar-special-refresh-commands 'dired-create-empty-file))

;; TODO: The below TODO is for dired-subtree, which I'm implicitly using because dired-sidebar requires it... should I just directly use-package dired-subtree? Even if I stop using dired-sidebar, I'm likely to keep using dired-subtree because it's cool
;; TODO: Maybe a command for contracting all trees would be nice

;; all-the-icons-dired
;; =====================
;; enable icons in dired
(use-package all-the-icons-dired
  :after (dired all-the-icons)
  :custom
  ;; colorize icons
  (all-the-icons-dired-monochrome nil)
  :hook (dired-mode . all-the-icons-dired-mode))

;; dired-fl
;; =====================
;; more beautiful colors in dired.
;; specific colors for file extensions and types.
(use-package diredfl
  :hook (dired-mode . diredfl-mode))


;; TODO: I haven't added a use-package definition for nerdicons... should I?
;; all-the-icons 
;; =====================
;; icon package
(use-package all-the-icons
  :if (display-graphic-p))

;; evil
;; =====================
;; VIM emulation layer
(use-package evil
  :init
  ;; these 2 are necessary for evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  ;; enable redo
  (evil-set-undo-system 'undo-redo))

;; evil-collection
;; =====================
;; a collection of improvements to provide VIM-like keybindings for many different
;; emacs features/modes
;; some features:
;; * vim-unimpaired keybindings
;; * hungry-delete integration with evil
(use-package evil-collection
  :after evil
  :ensure t
  :config
  ;; XXX: evil collection is pretty damn aggressive overriding keymaps
  ;; and you might not even get a change to define your own (it loads them at runtime
  ;; and maybe even several time, so it will override yours no matter what you do).
  ;; Be wary of disabling it when you find your keymaps won't work.
  ;; TODO: Maybe also disable ielm and eshell. Shell in vim-mode is pretty weird
  (delete 'dired evil-collection-mode-list)
  (evil-collection-init))

;; nerd-commenter emulation
(use-package evil-nerd-commenter
  :init
  (evilnc-default-hotkeys))

;; vim-surround emulation
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; vim match-it emulation
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

;; provides commands like `C-a` and `C-x` in vim to increase/decrease numbers
(use-package evil-numbers
  :general
  ;; TODO: Use embark for this!
  (:states '(normal insert)
           "C-c +" 'evil-numbers/inc-at-pt
           "C-c -" 'evil-numbers/dec-at-pt))

;; vim-snipe emulation, like `t` or `f` but uses two characters instead of one, so it's
;; more precise
;; TODO: Can I configure it so n and N also cycle results as for search?
(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  ;; snipe in whole buffer, not just current line
  (setq evil-snipe-scope 'whole-buffer))

;; gives an argument text-object so you can do `daa` for example to delete a whole argument
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
           "K" 'evil-jump-out-args)
  (:states 'motion
           "L" 'evil-forward-arg
           "H" 'evil-backward-arg))

;; TODO: There's an alternative package for this that does the same but it's not specifically for evil, right? what was its name? maybe ask chatgpt. UPDATE: It's volatile-highlights
;; TODO: Colors aren't very visible. This issue:
;; https://github.com/edkolev/evil-goggles/issues/33
;; says it's because doom-themes but I've tried changing
;; it with no result
;; pulses when providing feedback for many evil commands
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode))

;; hungry-delete
;; =====================
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
   hungry-delete-chars-to-skip " \t\r\f\v")
  (global-hungry-delete-mode))

;; TODO: There's an expand-region version that uses tree-sitter
;; expand-region
;; TODO: Maybe I should use combobulate when I can configure tree-sitter?
;; expand-region
;; =====================
;; increases selection progressively by syntactical units
(use-package expand-region
  :config
  ;; disable fast keys since my hydra does that and they conflict otherwise
  (setq expand-region-fast-keys-enabled nil))

;; lorem-ipsum
;; =====================
;; inserts filler text (lorem ipsum)
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

  :hook ((web-mode . haf/configure-html-lorem-ipsum)))


;; eglot
;; =====================
;; Built-in integration with the LSP protocol
(use-package eglot
  :config 
  ;; auto-save current buffer when any code action is executed.
  ;; it helps mainly with rust, since rust-analyzer only lints on save. 
  ;; this makes the warnings in the buffer obsolete, and it's especially
  ;; bothersome when I'm cycling through errors.
  ;; TODO: Maybe only set it for rust-mode?
  (advice-add 'eglot-code-actions :after #'(lambda (&rest r) (save-buffer)))

  (setq eglot-server-programs
        (append eglot-server-programs
                (list '(svelte-mode . ("svelteserver" "--stdio")) 
                      ;; TODO: rnix-lsp is hyper-deprecated. I have to use nil
                      '(nix-ts-mode . ("rnix-lsp"))
                      ;; TODO: Enable clippy with rust-analyzer, rust-analyzer itself has a guide to do it with eglot
                      '(rust-ts-mode . ("rust-analyzer")))))

  :hook ((tsx-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (svelte-mode . eglot-ensure)
         (nix-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         ;; auto-enable inlay hints
         (eglot-managed-mode . eglot-inlay-hints-mode)))

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
                 :args [])))

;; cape
;; =====================
;; provides some completion-at-point functions so it integrates very well
;; with corfu.
;; also has features to merge several capfs into a single one and to convert
;; company completions to native capfs
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

;; vertico
;; =====================
;; much better minibuffer completion
(use-package vertico
  :init
  (vertico-mode))

;; save history over emacs restarts, useful for vertico which sorts by it
(use-package savehist
  :ensure nil ;; already included in emacs
  :init
  (savehist-mode))

;; flymake
;; =====================
;; native frontend for static checkes that allows showing errors, collecting and navigating them
(use-package flymake
  :ensure nil ;; already included in emacs
  :init (setq flymake-no-changes-timeout 0.5))

;; sideline
;; =====================
;; visual frontend that can show many things in a sideline inlined in your current buffer
;; for example, it works with flymake errors
;; TODO: Use sideline-blame?
(use-package sideline
  :init 
  (setq sideline-display-backend-name t
        sideline-backends-right '(sideline-flymake))
  :hook ((flymake-mode  . sideline-mode)))
(use-package sideline-flymake)

;; diff-hl
;; =====================
;; shows git status in the gutter (uncommited changes)
;; TODO: Some keybindings or hydra to cycle across hunks
(use-package diff-hl
  :ensure t
  :hook 
  ;; enable it in dired
  ((dired-mode . diff-hl-dired-mode-unless-remote)
   ;; integrates it with magit
   (magit-pre-refresh  . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode)
  ;; show the hunk on clicking on the gutter
  :config (global-diff-hl-show-hunk-mouse-mode))

;; nix-ts-mode
;; =====================
;; nix syntax highlighting using built-in tree-sitter
(use-package nix-ts-mode
  :mode "\\.nix\\'")


;; XXX: TODO: Please note you may need clang to build rust's grammar. I may need to provide a message or something for all this information
;; FUTURE: This may not be needed in emacs 30 or further,
;;but currently, it's much easier this way
;; treesit-auto
;; =====================
;; autoinstalls tree-sitter grammars and maps non-tree-sitter modes to tree-sitter ones
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (add-to-list 'treesit-auto-langs 'nix)

  ;; set v0.20.2 version for typescript since the default is master and that's
  ;; incompatible with emacs' version
  (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
                                          :lang 'typescript
                                          :ts-mode 'typescript-ts-mode
                                          :remap 'typescript-mode
                                          :requires 'tsx
                                          :url "https://github.com/tree-sitter/tree-sitter-typescript"
                                          :revision "v0.20.2"
                                          :source-dir "typescript/src"
                                          :ext "\\.ts\\'"))
  ;; TODO: See if I can get treesit-auto to prompt to install it automatically
  ;; XXX: please note that treesit-auto doesn't appear to install it automatically
  ;; you have to run treesit-auto-install-all
  (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
                                          :lang 'nix
                                          :ts-mode 'nix-ts-mode
                                          :remap 'nix-mode
                                          ;; XXX: I had to fork it just to create a tag to a commit 
                                          ;; I knew was working for my emacs version
                                          :url "https://github.com/huuff/tree-sitter-nix"
                                          :revision "emacs-29.1.90"
                                          :ext "\\.nix\\'"))

  ;; auto switch to the treesitter mode for all langs included in treesit-auto-langs
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; smartparens
;; =====================
;; TODO: I don't know what it does and might even remove it
(use-package smartparens
  :defer t
  :hook (prog-mode . smartparens-mode)
  :config
  ;; load default config
  (require 'smartparens-config))

;; avy
;; =====================
;; powerful jumping anywhere in the frame (any window) with visual feedback
(use-package avy)

;; project
;; =====================
;; emacs' native project management that allows deciding which files belong to a single project
;; to allow project-wide searching, running commands, etc
(use-package project
  :ensure nil
  :init
  (setq 
   ;; allow creating a dummy, empty .project file to mark a project root
   ;; this is useful so it detects nested projects as independent projects
   project-vc-extra-root-markers '(".project")
   ;; ignore these directories (normally it gets them from .gitignore, but this is useful for nested projects)
   project-vc-ignores '("target/" "bin/" "obj/")))

(defun haf/project-remember-current-project ()
  "Remembers the current project"
  (interactive)
  (project-remember-project (project-current))
  ;; TODO: I could just do this with a simple substitution like (message "remembering project '%S'") or smth
  (message (concat "Remembering project '" (caddr (project-current)) "'")))

;; which-key
;; =====================
;; shows a pop-up window when you press a key that shows every possible
;; key you could follow it with and what command it runs
(use-package which-key
  :init
  ;; both of these lines enable compatibility with evil
  (setq which-key-allow-evil-operators t)
  (setq which-key-show-operator-state-maps t)
  :config
  (which-key-mode))

;; =====================
;; THEMES
;; =====================

(defun haf/switch-theme-by-mode (&optional args)
  "Switches theme depending on current major-mode"
  (interactive)
  (let ((next-theme (cl-case major-mode
                      ('rust-ts-mode 'doom-gruvbox)
                      ('emacs-lisp-mode 'doom-one)
                      ('typescript-ts-mode 'doom-material)
                      ('svelte-mode 'doom-moonlight)
                      (t nil))))
    (when next-theme
      (when (not (custom-theme-enabled-p next-theme)) (progn 
                                                        (dolist (theme custom-enabled-themes) (disable-theme theme))
                                                        (load-theme next-theme t))))))

;; doom-themes
;; =====================
;; a curated collection of nice themes
(use-package doom-themes
  :ensure t
  :config
  (setq 
   doom-themes-enable-bold t
   doom-themes-enable-italic t)
  (load-theme 'doom-one t)

  ;; enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; set theme by mode after a small delay so the mode
  ;; is initialized
  (haf/delay-fun #'haf/switch-theme-by-mode 2)

  ;; switch theme depending on language when the window changes, note that it only works here, and not in use-package's :hook
  (add-hook 'window-selection-change-functions #'haf/switch-theme-by-mode)
  (add-hook 'window-buffer-change-functions #'haf/switch-theme-by-mode))

;; solaire-mode
;; =====================
;; gives a brighter color to file-visiting buffers (where editing happens)
;; and a darker one to the rest, so the "main" buffer is highlighted
(use-package solaire-mode
  :config
  (solaire-global-mode +1))

;; dimmer
;; =====================
;; dims non-selected window so it's clearer which one is currently selected
(use-package dimmer
  :custom
  ;; a little darker than default
  (dimmer-fraction 0.3)
  ;; don't dim everything when the frame loses focus.
  ;; it's bothersome with two monitors
  (dimmer-watch-frame-focus-events nil)
  :init
  ;; these three defuns were copied off https://github.com/gonewest818/dimmer.el/issues/62#issuecomment-1820362245
  ;; for corfu integration
  (defun advise-dimmer-config-change-handler ()
    "Advise to only force process if no predicate is truthy."
    (let ((ignore (cl-some (lambda (f) (and (fboundp f) (funcall f)))
                           dimmer-prevent-dimming-predicates)))
      (unless ignore
        (when (fboundp 'dimmer-process-all)
          (dimmer-process-all t)))))
  (defun corfu-frame-p ()
    "Check if the buffer is a corfu frame buffer."
    (string-match-p "\\` \\*corfu" (buffer-name)))
  (defun dimmer-configure-corfu ()
    "Convenience settings for corfu users."
    (add-to-list
     'dimmer-prevent-dimming-predicates
     #'corfu-frame-p))
  ;; for eldoc-box integration
  (defun dimmer--is-eldoc-box ()
    (when (boundp 'eldoc-box--buffer)
      (equal eldoc-box--buffer (buffer-name))))
  :config
  ;; which-key integration
  (dimmer-configure-which-key)
  ;; hydra integration
  (dimmer-configure-hydra)
  ;; I don't even know whether I use posrframe, but configure its integration
  ;; just in case
  (dimmer-configure-posframe)
  ;; corfu integration
  (advice-add
   'dimmer-config-change-handler
   :override 'advise-dimmer-config-change-handler)
  (dimmer-configure-corfu)
  ;; eldoc-box integration
  (add-to-list 'dimmer-prevent-dimming-predicates
               #'dimmer--is-eldoc-box)
  ;; enable globally
  (dimmer-mode t))

;; modeline
;; =====================
;; much prettier and featurefull modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; TODO: Maybe also pulse for scroll and moves within windows
;; pulse
;; =====================
;; my own pulse configurations that I spent 2 hours tinkering without any satisfaction
(use-package pulse
  :ensure nil ;; already included in emacs
  :custom
  (pulse-delay 0.05)
  (pulse-iterations 5)
  :preface
  (defface haf/pulse-face
    '((t (:inherit region)))
    "Face for my pulse configurations")
  (defun haf/pulse-line (_)
    (pulse-momentary-highlight-one-line (point) 'haf/pulse-face))
  :config
  ;; pulse when changing windows
  (add-hook 'window-selection-change-functions #'haf/pulse-line)
  ;; pulse when changing buffers
  (add-hook 'window-buffer-change-functions #'haf/pulse-line))

;; direnv
;; =====================
;; automatically loads direnv config
(use-package direnv
  :config
  (direnv-mode))

;; web-mode
;; =====================
;; mode for editing web templates (svelte, vue, etc.) that mixes CSS, HTML and JS/TS modes
(use-package web-mode
  :config 
  ;; define a new mode for svelte so I can hook to it specifically instead of
  ;; every web-mode
  (define-derived-mode svelte-mode web-mode "Svelte")
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode)))

;; TODO: Also use "forge"?
;; TODO: It'd be great if i could use marginalia or something to put the last time a branch was updated next to it
;; magit
;; =====================
;; super-duper integration with git
(use-package magit)

;; eat (Emulate A Terminal)
;; =====================
;; fully-featured terminal emulator that interprets all escape sequences, allowing you to see
;; all colors, run full-screen programs, etc.
(use-package eat
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode))

;; hl-todo
;; =====================
;; highlights TODO, FIXME, MAYBE, OPT, etc. in comments
(use-package hl-todo
  :custom (hl-todo-keyword-faces
           '(("TODO"   . "#cc9393") 
             ("OPT"   . "#dc8cc3")
             ("MAYBE"   . "#7cb8bb")
             ("NOTE"   . "#d0bf8f")
             ("HACK"   . "#d0bf8f")
             ("TEMP"   . "#d0bf8f")
             ("FIXME"  . "#cc9393")
             ("XXXX*"  . "#cc9393")) "Mostly copy-pasted the default, but deleted some and added OPT and MAYBE")
  :config
  (global-hl-todo-mode))

;; hl-todo integration with magit, show TODOs also in magit buffers.
;; It's pretty cool, actually. It shows them in the status buffer and even shows you
;; how many were added in this branch from its parent
(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

;; navigate TODOs with consult
(use-package consult-todo :demand t)

;; keybindings
;; =====================
(general-create-definer normal-leader-bindings
  :states '(normal visual emacs)
  :prefix "SPC")

(normal-leader-bindings
  "e" '(hydra-flymake/body :which-key "Errors"))

;; TODO: for some of these (such as go to definition and go to implementation), a target is required (a workspace symbol). Wouldn't they be better as embark actions? UPDATE: I'm sure they exist as embark actions, but maybe I should fix keybindings
;; so they match these? such as embark-act + d for go to definition
(transient-define-prefix haf/language-transient ()
  "Transient for language-specific actions"
  [["Actions"
    :pad-keys t
    ("a" "Code actions" eglot-code-actions)
    ("r" "Rename" eglot-rename)
    ("f" "Format" eglot-format-buffer)
    ("o" "Organize imports" eglot-organize-imports :if (lambda () (fboundp 'eglot-organize-imports)))]
   ["Find"
    ;; TODO: Maybe also use xref-find-definitions? What's the difference?
    :pad-keys t
    ("d" "Declaration" eglot-find-declaration)
    ("i" "Implementation" eglot-find-implementation)
    ("u" "References" xref-find-references)
    ("m" "Outline" consult-imenu)]])

(transient-define-prefix haf/project-transient ()
  "Transient for project-wide actions"
  [["Project"
    :pad-keys t
    ("p" "Switch" project-switch-project)
    ("r" "Remember" haf/project-remember-current-project)]
   ["Find"
    :pad-keys t
    ("f" "File" consult-fd)
    ("F" "Text" consult-ripgrep)]
   ["Run"
    :pad-keys t
    ("c" "Compile" haf/compile-project)
    ("t" "Test" haf/run-project-tests)
    ("l" "Lint" haf/lint-project)]])

(transient-define-prefix haf/window-transient ()
  "Window transients"
  ["Windows"
   :pad-keys t
   ("C-t" "Toggle sidebar" dired-sidebar-toggle-sidebar)
   ("w" "Toggle popup" popper-toggle)
   ("t" "Cycle popup" popper-cycle)
   ("g" "Switch window" ace-window)])


(transient-define-prefix haf/autocomplete-transient ()
  "Transient autocompletions"
  ["Autocomplete"
   :pad-keys t
   ("TAB" "Normal" completion-at-point)
   ("f" "File" cape-file)
   ("w" "Dictionary" cape-dict)
   (":" "Emoji" cape-emoji)
   ("\\" "Tex" cape-tex)
   ("_" "Tex" cape-tex)
   ("^" "Tex" cape-tex)
   ("&" "SGML" cape-sgml)
   ("s" "Snippet" yasnippet-capf)])

(transient-define-prefix haf/tab-line-transient ()
  "Transient tab line"
  [["Switch"
    :pad-keys t
    :setup-children
    (lambda (_)
      (let ((tabs (haf/current-tabs)))
        (mapcar
         #'(lambda (i) (transient-parse-suffix
                        transient--prefix
                        `(,(number-to-string i)
                          ,(format "Tab %d" i)
                          (lambda () (interactive) (haf/switch-to-tab-index ,i)))))
         (number-sequence 1 (length tabs)))))]
   ["Tabs"
    :pad-keys t
    ("k" "Kill current" kill-current-buffer)
    ("K" "Kill other" haf/tab-line-close-other-tabs)
    ("<right>" "Next" tab-line-switch-to-next-tab :transient t)
    ("<left>" "Previous" tab-line-switch-to-prev-tab :transient t)]])


(transient-define-prefix haf/transient ()
  "Prefix for all of my own keybindings"
  [["Editing"
    :pad-keys t
    ("TAB" "Autocomplete" haf/autocomplete-transient)
    ("C-w" "Expand region" haf/expand-and-start-region-hydra)
    ("C-d" "Create cursor" haf/next-cursor-and-start-region-hydra)
    ("C-s" "Jump" avy-goto-char-timer)]
   ["Views"
    :pad-keys t
    ("w" "Windows" haf/window-transient)
    ("t" "Tabs" haf/tab-line-transient)]
   ["Actions"
    :pad-keys t
    ("p" "Project" haf/project-transient)
    ("l" "Language" haf/language-transient)]])

(general-define-key
 :states '(normal visual insert motion)
 :keymaps 'override
 "C-z" 'haf/transient)
