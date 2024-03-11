(transient-define-prefix haf/transient ()
  "Prefix for all of my own keybindings"
  [["Editing"
    :pad-keys t
    ("TAB" "Autocomplete" haf/autocomplete-transient)
    ("C-w" "Expand region" haf/expand-and-start-region-transient)
    ("C-d" "Create cursor" haf/next-cursor-and-start-region-transient)
    ("C-s" "Jump" avy-goto-char-timer)]
   ["Views"
    :pad-keys t
    ("w" "Windows" haf/window-transient)
    ("t" "Tabs" haf/tab-line-transient)
    ("'" "Font size" global-text-scale-adjust)]
   ["Actions"
    :pad-keys t
    ("p" "Project" haf/project-transient)
    ("l" "Language" haf/language-transient)
    ("e" "Errors" haf/start-error-transient)
    ("g" "Git" haf/git-transient)
    ("!" "REPL" haf/repl-transient)]])

(general-define-key
 :states '(normal visual insert motion)
 :keymaps 'override
 "C-z" 'haf/transient)

(defun haf/transient-quit ()
  "Dummy function that does nothing so I can use for exiting transients"
  (interactive))


;; ================
;; REPL TRANSIENT
;; ================
;; TODO: It'd be great to also have some calculator REPL
(defun haf/pop-eshell-buffer ()
  "Opens an eshell buffer in the current project"
  (interactive)
  (pop-to-buffer (get-buffer-create "*eshell*"))
  (project-eshell))

(defun haf/pop-ielm-buffer ()
  "Opens an eshell buffer in the current project"
  (interactive)
  (pop-to-buffer (get-buffer-create "*ielm*"))
  (ielm))

(transient-define-prefix haf/repl-transient ()
  "Transient for REPLs"
  ["REPL"
   :pad-keys t
   ("!" "Terminal" haf/pop-eshell-buffer)
   ("e" "Emacs" haf/pop-ielm-buffer)])

;; ================
;; END OF REPL TRANSIENT
;; ================

(transient-define-prefix haf/git-transient ()
  "Transient for Git actions"
  [["Git"
    :pad-keys t
    ("g" "Magit" magit)]
   ;; TODO: Can't I just remove the diff-hl transient and override it with mine? I'd have to remove the keymap and
   ;; the footer
   ["Hunks"
    :pad-keys t
    ("n" "Next" diff-hl-show-hunk-next)
    ("p" "Previous" diff-hl-show-hunk-previous)]])


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
    ;; TODO: Limit to 9 tabs, otherwise it breaks
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

;; ================
;; REGION/CURSOR TRANSIENT
;; ================

;; TODO: Can I make any keybindings not expressed here to go through to the actual command, like hydra does?
;; Otherwise, when I have multiple cursors, pressing "C" does nothing (I have to press q first to exit the transient
;; to change the text)
(transient-define-prefix haf/region-transient ()
  "Transient for expanding regions and adding cursors"
  [["Region"
    :pad-keys t
    ("+" "Expand" er/expand-region :transient t)
    ("-" "Contract" er/contract-region :transient t)
    ("C-w" "Expand" er/expand-region :transient t)]
   ["Cursors"
    :pad-keys t
    ("n" "Next" haf/add-next-multicursor :transient t)
    ("N" "Previous" haf/remove-previous-multicursor :transient t)
    ("C-d" "Next" haf/add-next-multicursor :transient t)
    ("t" haf/toggle-multicursor-package
     :description (lambda () (format "Package (%s)" (symbol-name haf/multicursor-package)))
     :transient t)]
   ["Quit"
    ;; TODO: maybe make q exit multiple cursors and remove region for all packages
    :pad-keys t
    ("q" "Quit" haf/transient-quit)]])

(defun haf/expand-and-start-region-transient ()
  "Expands region and runs the 'region-transient'."
  (interactive)
  (er/expand-region 1)
  (haf/region-transient))

(defun haf/next-cursor-and-start-region-transient ()
  "Adds a multicursor and runs the 'region-transient'"
  (interactive)
  (haf/add-next-multicursor)
  (haf/region-transient))

;; ================
;; END OF REGION/CURSOR TRANSIENT
;; ================

;; =================
;; FLYMAKE TRANSIENT
;; =================
;; TODO: Once there is more than one flymake diagnostics buffer, this won't be able to complete a name (try-completion only finds a prefix) and thus won't close the window
(defun haf/close-flymake-diagnostics ()
  "Close the window on flymake diagnostics"
  (interactive)
  (quit-windows-on (try-completion "*Flymake diagnostics for" (mapcar #'buffer-name (buffer-list)))))

(defun haf/start-error-transient ()
  "Starts the error transient, opens the flymake diagnostics and goes to the next error"
  (interactive)
  (flymake-show-buffer-diagnostics)
  (flymake-goto-next-error)
  (haf/error-transient))

(defun haf/close-flymake-and-open-consult ()
  "Closes flymake diagnostics and opens consult-flymake"
  (interactive)
  (haf/close-flymake-diagnostics)
  (consult-flymake))

(defun haf/quickfix-and-next-error ()
  "Tries to quickfix current error and then goes to the next one"
  (interactive)
  (eglot-code-action-quickfix (point))
  (flymake-goto-next-error))

;; TODO: Allow switching to project diagnostics
;; TODO: Different keybindings for errors and warnings/notes (prefix goes to these for flymake-goto-«next or previous»-error)
(transient-define-prefix haf/error-transient ()
  "Transient for jumping around errors and fixing them"
  ["Errors"
   :pad-keys t
   ("<right>" "Next" flymake-goto-next-error :transient t)
   ("<left>" "Previous" flymake-goto-prev-error :transient t)
   ("f" "Quickfix" haf/quickfix-and-next-error :transient t)
   ("c" "Consult" haf/close-flymake-and-open-consult)
   ("q" "Close" haf/close-flymake-diagnostics)])

;; =================
;; END OF FLYMAKE TRANSIENT
;; =================
