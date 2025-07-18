;; TODO: Some nice config for follow-mode (see https://mbork.pl/2024-03-18_Follow_mode )
;; TODO: Maybe an activity for elfeed?
;; TODO: Use the elisp tree sitter
;; TODO: Try to get a nice activities.el + verb.el configuration
;; TODO: Use mu4e
;; TODO: Add jira?
;; TODO: Some mode or configuration to directly interact with a SQL database
;; TODO: Use oauth2.el (https://elpa.gnu.org/packages/oauth2.html ) with verb.el for my work duties
;; TODO: Something like verb.el, but for graphql
;; TODO: Remove tabs from *dape-* buffers
;; TODO: Can I make magit close the status buffer automatically after a push?
;; TODO: Maybe use prodigy
;; TODO: Maybe use golden-ratio
;; TODO: Perhaps I could use aggressive-indent-mode for elisp only
;; TODO: Maybe I should bring back evil-collection for dired and try to get used to wdired instead
;; TODO: Use justl mode
;; TODO: Install eglot-signature-eldoc-talkative
;; TODO: Maybe use evil-quickscope
;; TODO: Maybe use no-littering?
;; TODO: repl-driven-development might be incredibly cool
;; TODO: I did rebind k to eldoc-box, but I'd like being able to open eldoc in a separate window split like I did before. I don't even remember what the actual command was, so maybe I should unbind it and try `C-h k k` to find it
;; TODO: Use eglot-x?
;; TODO: I think any errors during nix-rebuild that say "assignment to free variable" mean that I'm assigning to variables that don't even exist
;; TODO: Use meow? It seems pretty rad
;; TODO: Some evil-mc keybindings for creating a cursor on each line beginning/end
;; TODO: Maybe set-up some code folding. UPDATE: Theres a cool ts-fold package that does folding with treesitter, but I don't think it works with the builtin treesitter so I may need to wait for a next release
;; TODO: There's some error that appears when building it with nix, build with -L to find out what it is
;; TODO: A transient to interactively indent/deindent visually selected regions without losing the selection
;; TODO: Can I make some packages load lazily with :command? Is it worth it?
;; TODO: I'd love to use project-x, but it's not on MELPA
;; TODO: Enable the daemon mode
;; TODO: Maybe check out whether I want some corfu extensions (see https://github.com/minad/corfu#extensions )
;; TODO: follow this config a little https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/ 
;; TODO: Entire buffer textobj would be nice, I do `cae` or `dae` a lot in vim
;; TODO: Try to split some sections to different files
;; TODO: There are two commands I need to run so fonts work. Is there any way I could automate it or notify whether it's needed?:
;; - nerd-icons-install-fonts
;; - all-the-icons-install-fonts
;; TODO: Indent guides for YAML and python (https://github.com/jdtsmith/indent-bars )
;; TODO: Some way to go back to the previous buffer for when I'm switching between projects
;; TODO: A config to go to "alternate files", such as, for example, going to the test, or the the css module of a file
;; TODO: Maybe set up dictionaries and spell checking?
;; TODO: Maybe use bufler? It's pretty cool, but I'd love to have a preview (consult) for switching buffers. Also, workspaces seem pretty sensible and I could use them for my tab-line
;; TODO: If I can use emacs-sideline to show the function signature with eldoc on the left, that's be really cool
;; TODO: consult for diff-hl hunks?
;; TODO: Maybe use consult-web?
;; TODO: ignore anything under .gitignore for consult-todo
;; TODO: Maybe use wgrep?
;; TODO: A keybinding or something to replace selected text with clipboard
;; TODO: Maybe bring back rustic, since it has a new maintainer (https://github.com/emacs-rustic/rustic )
;; TODO: bring back cc-isearch-menu
;; TODO: maybe bring back casual?

;; refresh open buffers when filesystem changes
(global-auto-revert-mode)

;; makes links clickable
(global-goto-address-mode)

;; stores window configurations so after any changes (closing windows)
;; you can call winner-undo to go back to the previous state
;; I use it to maximize/minimize windows
(winner-mode)

;; disable ugly top toolbars and scroll bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; put backup files in a backup directory instead of the same directory as the source file
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; maximum highlighting with tree-sitter
(setq treesit-font-lock-level 4)

;; do not use tabs for indenting (only spaces)
(setq-default indent-tabs-mode nil)

;; show line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

;; set font 
(set-frame-font "Fira Code 10" nil t)

;; XXX I'm not even entirely sure what this does. But I use it so directories
;; in dired are actually sorted alphabetically. Dired uses `ls` under the covers, and
;; apparently this env var influences `ls`'s listing order. Hopefully it won't affect anything
;; else unexpected.
;; I'm mainly interested in it because sveltekit names special files with `+` prefixes so they appear
;; on top, but the default order ignores this.
(setenv "LC_ALL" "C")

(defcustom haf/keybinding-mode 'evil
  "The keybinding mode to use, either 'evil, 'emacs or 'meow"
  :type '(choice (const :tag "evil" evil)
                 (const :tag "emacs" evil-multiedit)
                 (const :tag "meow" meow)))

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


;; tempel
;; ====================
;; snippets
(use-package tempel
  :after bind-key
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; go to next placeholder in template with TAB like for yasnippet
  :bind (:map tempel-map ([tab] . tempel-next))

  ;; TODO: Can I enable it but make it trigger only with the trigger-prefix?
  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;; use tempel for LSP templates in eglot (by default eglot forces yasnippet)
(use-package eglot-tempel
  :ensure t
  :after (eglot tempel)
  :init (eglot-tempel-mode))


;; electric-pair-mode
;; ====================
;; auto inserts closing braces, parentheses and quotes
(use-package elec-pair
  :ensure nil ;; already included in emacs
  :config
  (electric-pair-mode))

;; super-save
;; =====================
;; auto-saves the buffer to its file on certain events (such as switching buffer)
(use-package super-save
  :ensure t
  :custom
  (super-save-auto-save-when-idle t "Auto-save when idle")
  :config
  (super-save-mode +1))

;; bind-key
;; =====================
;; use-package needs it for its :bind configuration
(use-package bind-key
  :ensure t
  :config
  (add-to-list 'same-window-buffer-names "*Personal Keybindings*"))


;; dashboard
;; =====================
;; nice dash board for the first screen
(use-package dashboard
  :ensure t
  :after s ;; I need it for my custom widget
  :preface
  (defun haf/switch-project-and-open-sidebar (project)
    "Switches project.el project (by finding file) and opens dired-sidebar"
    (let ((default-directory project))
      (progn
        (project-find-file)
        (dired-sidebar-show-sidebar))))
  (defun haf/elfeed-entries (limit)
    (let (entries)
      ;; XXX: I outright had to inline (aka copy-paste) the with-elfeed-db-visit macro
      ;; in order to get this to work. Otherwise, this always gave an obscure error
      (catch 'elfeed-db-done
        (prog1 nil
          (elfeed-db-ensure)
          (avl-tree-mapc
           (lambda (id)
             (let* ((entry (elfeed-db-get-entry id))
                    (tags (elfeed-entry-tags entry)))
               (when (and (member 'emacs tags) (member 'unread tags))
                 (push entry entries))
               (when (>= (length entries) limit)
                 ;; XXX: I also had to inline elfeed-db-return here because for some reason nothing works!
                 (throw 'elfeed-db-done nil))))
           elfeed-db-index)))
      (nreverse entries)))
  (defun haf/dashboard-insert-elfeed (list-size)
    "Add the list of LIST-SIZE items of RSS entries."
    (elfeed-update)
    (let ((entries (haf/elfeed-entries list-size)))
      (dashboard-insert-section
       ;; I have to add the icon here because sections are hardcoded in dashboard-insert-heading
       (concat (all-the-icons-faicon "rss") " RSS (Updated " (format-time-string "%Y-%m-%d %H:%M:%S" (elfeed-db-last-update)) "):")
       entries
       list-size
       'elfeed
       (dashboard-get-shortcut 'elfeed)
       `(lambda (&rest _) (elfeed-search-show-entry ,el))
       (let* ((title (elfeed-entry-title el))
              (date (format-time-string "%Y-%m-%d" (elfeed-entry-date el)))
              (feed-name (elfeed-feed-title (elfeed-entry-feed el)))
              (max-title-width 50)
              (truncated-title (s-truncate max-title-width title))
              ;; TODO: Can I make this depend on the longest line in the buffer?
              (right-align (s-repeat (- (+ max-title-width 15) (string-width truncated-title)) " "))) 
         (concat
          (propertize date 'face 'elfeed-search-date-face)
          " "
          truncated-title
          right-align
          (propertize (format "(%s)" feed-name) 'face 'elfeed-search-feed-face))))))
  
  :custom
  (dashboard-projects-backend 'project-el "Choose project.el instead of projectile for the project list")
  (dashboard-items '((projects . 7)
                     (recents  . 5)) "Widget order and size. (projects . 5) means to show 5 projects")
  (dashboard-icon-type 'all-the-icons "Show icons with all-the-icons")
  (dashboard-display-icons-p t "Show icons")
  (dashboard-set-heading-icons t "Show icons in widget headings")
  (dashboard-set-file-icons t "Show icons for files")
  (dashboard-center-content t "Horizontally center widgets")
  (dashboard-startup-banner 'logo "Show an alternative, modern, emacs logo.")
  (dashboard-projects-switch-function 'haf/switch-project-and-open-sidebar "Open sidebar when opening a project")
  (dashboard-startupify-list '(
                               dashboard-insert-banner
                               dashboard-insert-newline
                               dashboard-insert-banner-title
                               dashboard-insert-newline
                               dashboard-insert-navigator
                               dashboard-insert-newline
                               dashboard-insert-init-info
                               dashboard-insert-newline
                               dashboard-insert-items
                               dashboard-insert-newline
                               dashboard-insert-footer))
  (dashboard-navigator-buttons
   `(;; line1
     ((,(all-the-icons-faicon "folder" :height 1.1 :v-adjust 0.0)
       "Open"
       "Open file"
       (lambda (&rest _) (call-interactively #'find-file)))
      (,(all-the-icons-faicon "sticky-note" :height 1.1 :v-adjust 0.0)
       "Scratch"
       "Open scratch buffer"
       (lambda (&rest _) (scratch-buffer))))))
  :config
  ;; add my elfeed widget
  (add-to-list 'dashboard-item-shortcuts '(elfeed . "f"))
  (add-to-list 'dashboard-item-generators  '(elfeed . haf/dashboard-insert-elfeed))
  (add-to-list 'dashboard-items '(elfeed . 10) t)
  ;; fix the jump to section for my elfeed section with evil
  ;; everything about this is an absolute clusterfuck
  ;; sections are hardcored, and dashboard sets keybinding at a weird time
  ;; check out 'evil-dashboard-setup-jump-commands' if you want an explanation for this weirdness
  (advice-add 'dashboard-insert-startupify-lists :after
              (lambda ()
                (evil-collection-define-key 'normal 'dashboard-mode-map "f" (symbol-function (lookup-key dashboard-mode-map "f")))))
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
  :general
  (:states '(normal insert)
           :keymaps 'override
           "C-SPC" 'completion-at-point)
  :custom
  (corfu-auto t "Enable autocompletion")
  (corfu-auto-delay 0.75 "Time to wait before autocompletion"))

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
  (completion-styles '(orderless basic) "Use orderless completion by default and basic as a fallback")
  (completion-category-overrides '((file (styles basic partial-completion))) "I don't know exactly what this does, but the Orderless' README recommends it. Apparently is so hostname completion works for TRAMP"))

;; move-text
;; ====================
;; move text around like in vscode
(use-package move-text
  :ensure t
  :if (not (eq haf/keybinding-mode 'evil))
  :config
  ;; default bindings: M-up and M-down
  (move-text-default-bindings)

  (advice-add 'move-text-up :after 'haf/indent-region)
  (advice-add 'move-text-down :after 'haf/indent-region))

;; drag-stuff
;; =====================
;; move text around like in vscode
;; this is like move-text, but works in evil-mode
(use-package drag-stuff
  :ensure t
  :if (eq haf/keybinding-mode 'evil)
  :config
  ;; enable it
  (drag-stuff-global-mode 1)
  ;; enable default keybindings like M-up, M-down
  (drag-stuff-define-keys)
  (advice-add 'drag-stuff-up :after 'haf/indent-region)
  (advice-add 'drag-stuff-down :after 'haf/indent-region))

;; eldoc
;; =====================
;; buil-in documentation browsing mode
(use-package eldoc
  :ensure nil ;; already included in emacs
  :custom
  (eldoc-idle-delay 0.75 "Seconds to wait before showing help in the echo area"))

;; displays eldoc in a floating childframe
(use-package eldoc-box
  :after eldoc
  :general
  (:states 'normal
           :keymaps 'override
           ;; same key as for vim
           "K" 'eldoc-box-help-at-point)
  :custom
  (eldoc-box-clear-with-C-g t "Allow closing the popup with C-g"))

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

;; TODO: When I do consult-line-multi, I'd absolutely love being able to
;; use embark like I do for consult-file to open the file in the other window
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

;; wgrep
;; =====================
;; allow editing search results directly, across the whole project
(use-package wgrep)

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
  ("C-," 'embark-act)
  :preface
  ;; XXX: Most of this is copy-pasted off https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
  ;; to enable integration with which-key
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))


  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))
  :config
  ;; set-up which-key integration
  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

;; consult integration
(use-package embark-consult
  :after (embark consult)
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; TODO: I use diffhl to highlight vc-changed files, but this doesn't look that cool and
;; doesn't integrate that well with dired-subtree.. maybe I should use dired-rainbow to configure that?
;; =====================
;; the built-in file-manager
(use-package dired
  :ensure nil ;; already included in emacs
  ;; TODO: Maybe this cold be a little bit brighter?
  ;; TODO: This gets lost after some seconds... can I fix that?
  ;; mark with the whole current line in dired
  :hook (dired-mode . hl-line-mode)
  :custom
  (dired-listing-switches "-aBhl  --group-directories-first" "Sort so directories are on top")
  :general
  (:keymaps '(dired-mode-map)
            :states '(normal)
            ;; TODO: It'd be cool to have a dired-toggle-mark command that just toggles
            ;; rather than having two separate keys
            "i" 'wdired-change-to-wdired-mode
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


;; TODO: Infuriatingly, forward-history for dired-rename gives the wrong path instead of the current file's path
;; TODO: Exiting wdired goes back to dired-mode, not dired-sidebar-mode. There is an advice for this (dired-sidebar-wdired-change-to-wdired-mode-advice) but it doesn't seem to be working
;; dired-sidebar
;; =====================
;; use dired as a tree directory explorer in a sidebar just like a real IDE
(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar dired-sidebar-show-sidebar)
  :custom
  (dired-sidebar-should-follow-file t "Follow currently-open file in the sidebar")
  :config
  ;; auto-refresh sidebar when running any dired-ranger commands
  (add-to-list 'dired-sidebar-special-refresh-commands 'dired-ranger-move)
  (add-to-list 'dired-sidebar-special-refresh-commands 'dired-ranger-paste)
  (add-to-list 'dired-sidebar-special-refresh-commands 'dired-create-empty-file)
  ;; refresh sidebar after exiting dired without changes (otherwise dired-subtree breaks)
  (add-to-list 'dired-sidebar-special-refresh-commands 'wdired-abort-changes)
  ;; show sidebar when switching project
  (advice-add 'project-switch-project :after #'(lambda (&rest _) (dired-sidebar-show-sidebar))))

;; TODO: Maybe a command for contracting all trees would be nice
(use-package dired-subtree)

;; all-the-icons-dired
;; =====================
;; enable icons in dired
(use-package all-the-icons-dired
  :after (dired all-the-icons)
  :custom
  (all-the-icons-dired-monochrome nil "Colorize icons")
  :hook (dired-mode . all-the-icons-dired-mode))

;; dired-fl
;; =====================
;; more beautiful colors in dired.
;; specific colors for file extensions and types.
(use-package diredfl
  :after (dired)
  :hook (dired-mode . diredfl-mode))


;; all-the-icons 
;; =====================
;; icon package
(use-package all-the-icons
  :if (display-graphic-p))

;; nerd-icons
;; =====================
;; another icon package
;; I don't know what for, but I'm sure I use it for something but as a
;; transitive dependency. So I'm adding it here
(use-package nerd-icons)

;; evil
;; =====================
;; VIM emulation layer
(use-package evil
  :if (eq haf/keybinding-mode 'evil)
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
  (delete 'dired evil-collection-mode-list)
  ;; I find vim keybindings uncomfortable in the shell
  (delete 'eshell evil-collection-mode-list)
  (evil-collection-init))

;; nerd-commenter emulation
(use-package evil-nerd-commenter
  :if (eq haf/keybinding-mode 'evil)
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
  ;; TODO: Use embark for this?
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
  (evil-snipe-override-mode +1))

;; gives an argument text-object so you can do `daa` for example to delete a whole argument
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
  :custom
  (hungry-delete-join-reluctantly t "Do not join words. Without it, deleting between 'word1       word2' would give 'word1word2', but with it, it's 'word1 word2'") 
  ;; copied off https://github.com/nflath/hungry-delete/issues/20
  (hungry-delete-chars-to-skip " \t\r\f\v" "Prevents it from being too hungry and deleting all newlines and tabs")
  :config
  (global-hungry-delete-mode))

;; TODO: There's an expand-region version that uses tree-sitter
;; TODO: Maybe I should use combobulate when I can configure tree-sitter?
;; expand-region
;; =====================
;; increases selection progressively by syntactical units
(use-package expand-region
  :custom
  (expand-region-fast-keys-enabled nil "Disable fast keys since I do the same thing with a transient"))


;; lorem-ipsum
;; =====================
;; inserts filler text (lorem ipsum)
;; TODO: Can I configure an abbrev for this? (so writing lorem would expand to a paragraph) That's be cool. UPDATE: I've done it with tempel but I haven't linked that to abbrev yet
(use-package lorem-ipsum
  :commands (lorem-ipsum-insert-sentences lorem-ipsum-insert-list lorem-ipsum-insert-paragraphs)
  :preface 
  (defun haf/configure-lorem-ipsum ()
    (cond
     ((derived-mode-p 'web-mode) (setq
                                  lorem-ipsum-paragraph-separator "<br><br>\n"
                                  lorem-ipsum-sentence-separator "&nbsp;&nbsp;"
                                  lorem-ipsum-list-beginning "<ul>\n"
                                  lorem-ipsum-list-bullet "<li>"
                                  lorem-ipsum-list-item-end "</li>\n"
                                  lorem-ipsum-list-end "</ul>\n"))
     ((derived-mode-p 'markdown-mode) (setq
                                       lorem-ipsum-paragraph-separator "\n\n"
                                       lorem-ipsum-sentence-separator " "
                                       lorem-ipsum-list-beginning "\n"
                                       lorem-ipsum-list-bullet "* "
                                       lorem-ipsum-list-item-end "\n"
                                       lorem-ipsum-list-end "\n"))
     ((derived-mode-p 'text-mode) (setq
                                   lorem-ipsum-paragraph-separator "\n\n"
                                   lorem-ipsum-sentence-separator " "
                                   lorem-ipsum-list-beginning "\n"
                                   lorem-ipsum-list-bullet "- "
                                   lorem-ipsum-list-item-end "\n"
                                   lorem-ipsum-list-end "\n"))))

  :hook ((web-mode . haf/configure-lorem-ipsum)
         (markdown-mode . haf/configure-lorem-ipsum)
         (text-mode . haf/configure-lorem-ipsum)))


;; eglot
;; =====================
;; Built-in integration with the LSP protocol

(use-package eglot
  :config 
  ;; save on code actions, I use it for rust-analyzer because it only runs some checks on save
  ;; note that super-save-triggers does not work for this
  (advice-add 'eglot-code-actions :after #'(lambda (&rest r) (save-buffer)))

  ;; setup LSPs
  (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))
  (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) . ("nil")))
  (add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) . ("rust-analyzer" :initializationOptions
                                                                    (:cargo (:features "all" :targetDir t)
                                                                            :check (:command "clippy" :features "all")))))
  
  ;; these two make eglot faster according to
  ;; https://www.reddit.com/r/emacs/comments/1aw6xkc/comment/kriu3ye/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  (setq eglot-events-buffer-size 0)
  (fset #'jsonrpc--log-event #'ignore)
  
  :hook ((tsx-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (svelte-mode . eglot-ensure)
         (nix-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         ;; auto-enable inlay hints
         (eglot-managed-mode . eglot-inlay-hints-mode)))

(use-package eglot-booster
  :ensure nil ;; I just downloaded it
  :after eglot
  :config (eglot-booster-mode))

;; apheleia
;; =====================
;; auto-format buffers on save
(use-package apheleia
  :config
  (push '(djlint-jinja . ("djlint"
                          "--reformat"
                          "--profile=jinja"
                          "-"))
        apheleia-formatters)
  (push '(leptosfmt . ("leptosfmt" "--stdin" "--rustfmt")) apheleia-formatters)
  (add-to-list 'apheleia-mode-alist '(jinja-web-mode . djlint-jinja))
  (apheleia-global-mode +1))

;; dtrt-indent
;; =====================
;; auto-configures indentation settings from file
;; should fix my woes with JSX/Svele files
(use-package dtrt-indent
  :init (dtrt-indent-global-mode 1))

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
                 :program (lambda ()
                            (file-name-concat "target" "debug"
                                              (thread-first (dape-cwd)
                                                            (directory-file-name)
                                                            (file-name-split)
                                                            (last)
                                                            (car))))
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
(use-package sideline-flymake
  :after sideline)

;; diff-hl
;; =====================
;; shows git status in the gutter (uncommited changes)
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


;; XXX: Please note you may need clang to build rust's grammar. I may need to provide a message or something for all this information
;; FUTURE: This may not be needed in emacs 30 or further,
;; but currently, it's much easier this way
;; treesit-auto
;; =====================
;; autoinstalls tree-sitter grammars and maps non-tree-sitter modes to tree-sitter ones
(use-package treesit-auto
  :custom
  ;; TODO: docstring
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
  (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
                                          :lang 'typescript
                                          :ts-mode 'typescript-ts-mode
                                          :remap 'typescript-mode
                                          :requires 'tsx
                                          :url "https://github.com/tree-sitter/tree-sitter-typescript"
                                          :revision "v0.20.2"
                                          :source-dir "typescript/src"
                                          :ext "\\.ts\\'"))
  (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
                                          :lang 'javascript
                                          :ts-mode 'js-ts-mode
                                          :remap '(js-mode javascript-mode js2-mode)
                                          :url "https://github.com/tree-sitter/tree-sitter-javascript"
                                          :revision "v0.20.1"
                                          :source-dir "src"
                                          :ext "\\.jsx?\\'"))
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


;; TODO: karthinks has a cool config for desaturating the screen when jumping with avy but
;; I haven't seen it... how can I do that? Search "karthinks avy" to see his notes on it
;; UPDATE: ace-window has that, so I could just copy it
;; avy
;; =====================
;; powerful jumping anywhere in the frame (any window) with visual feedback
(use-package avy)

;; link-hint
;; ====================
;; avy interface for highlighting and jumping to/opening links in buffer
(use-package link-hint
  :ensure t)

;; ace-window
(use-package ace-window
  :ensure t
  :custom
  (aw-dispatch-always t))

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
   project-vc-extra-root-markers '(".project")))

(defun haf/project-remember-current-project ()
  "Remembers the current project"
  (interactive)
  (project-remember-project (project-current))
  (message (format "Remembering project '%s'" (caddr (project-current)))))

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
                      (rust-ts-mode 'doom-gruvbox)
                      (emacs-lisp-mode 'doom-one)
                      (nix-ts-mode 'doom-city-lights)
                      (typescript-ts-mode 'doom-dark+)
                      (js-ts-mode 'doom-dark+)
                      (svelte-mode 'doom-moonlight)
                      (sql-mode 'doom-vibrant)
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
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
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
  :preface
  (defun haf/solaire-mode-real-buffer-p ()
    "My custom override to decide where to apply solaire"
    ;; do not apply in dashboard, it makes the logo look ugly
    (or (eq major-mode 'dashboard-mode)
        (solaire-mode-real-buffer-p)))
  :custom
  (solaire-mode-real-buffer-fn #'haf/solaire-mode-real-buffer-p "My custom rules to decide where to apply solaire") 
  :config
  (solaire-global-mode +1))

;; dimmer
;; =====================
;; dims non-selected window so it's clearer which one is currently selected
(use-package dimmer
  :custom
  (dimmer-fraction 0.3 "A little darker than default")
  (dimmer-watch-frame-focus-events nil "Don't dim everything when the frame loses focus. It's bothersome with two monitors")
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
  ;; I don't even know whether I use postrame, but configure its integration
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
  (pulse-delay 0.05 "Seconds before each iteration")
  (pulse-iterations 5 "Number of iterations")
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

;; envrc
;; =====================
;; automatically loads direnv config
(use-package envrc
  :hook (after-init . envrc-global-mode))

;; web-mode
;; =====================
;; mode for editing web templates (svelte, vue, etc.) that mixes CSS, HTML and JS/TS modes
(use-package web-mode
  :config 
  ;; define a new mode for svelte so I can hook to it specifically instead of
  ;; every web-mode
  (define-derived-mode svelte-mode web-mode "Svelte")
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode))
  (define-derived-mode jinja-web-mode web-mode "Jinja")
  (add-to-list 'auto-mode-alist '("\\.html.jinja\\'" . jinja-web-mode)))

;; TODO: Also use "forge"?
;; TODO: It'd be great if i could use marginalia or something to put the last time a branch was updated next to it
;; magit
;; =====================
;; super-duper integration with git
(use-package magit
  :custom
  (magit-list-refs-sortby "-creatordate" "Sort revs by least recently used instead of alphabetically"))

;; eat (Emulate A Terminal)
;; =====================
;; fully-featured terminal emulator that interprets all escape sequences, allowing you to see
;; all colors, run full-screen programs, etc.
(use-package eat
  :config
  ;; makes EAT quicker according to
  ;; https://www.reddit.com/r/emacs/comments/1aw6xkc/comment/kriu3ye/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  (setq process-adaptive-read-buffering nil) 
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode))

;; TODO: Maybe some keybindings for hl-todo and consult-todo
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


;; verb
;; =====================
;; postman-like HTTP client in emacs
(use-package verb)

;; org
;; =====================
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;; activities
;; =====================
;; stores specific windows configurations as tabs
;; I use it to split my day-to-day file editing from email/restclient/etc.
(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))

;; markdown-mode
;; =====================
;; self-explanatory
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;; TODO: Maybe try to set up elfeed-summary as a sidebar on an activity?
;; elfeed
;; =====================
;; RSS reader in emacs
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
        '(("https://planet.emacslife.com/atom.xml" emacs)
          ("https://www.reddit.com/r/emacs/.rss" emacs))))

;; anzu
;; =====================
;; current match and total matches in mode line for searches
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode +1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package evil-anzu
  :ensure t
  :after (evil anzu))

;; persistent-scratch
;; =====================
;; keep your scratch session between restarts
(use-package persistent-scratch
  :ensure t
  :config (persistent-scratch-setup-default))

;; literate-calc-mode
;; ====================
;; it's calc mode but solutions are given as inlay hints in a buffer
;; instead of that weird stack stuff
(use-package literate-calc-mode
  :ensure t
  :hook (literate-calc-mode . display-line-numbers-mode))

;; TODO: Comment explaining auctex
;; Use C-c C-c to compile latex and then C-c C-v to view the output
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        ;; use pdf-tools to open pdfs
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  ;; update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

;; graphql-mode
;; ====================
;; TODO I'm not entirely sure what it does yet but I know it allows sending queries and maybe it has something else
(use-package graphql-mode)

;; I set the background black in early-init so it's not flashing white
;; while starting up. This, however, breaks the mouse pointer, making it full-black.
;; I force it to be white after everything has loaded to fix it.
(set-mouse-color "#FFFFFF")
