(defvar multicursor-package 'multiple-cursors
  "The package to be used for multiple cursors, either 'evil-mc or 'multiple-cursors")

(use-package hydra
  :config
  ;; TODO: Allow switching to project diagnostics
  ;; TODO: Different keybindings for errors and warnings/notes (prefix goes to these for flymake-goto-«next or previous»-error)
  (defhydra hydra-flymake
    (:pre (progn (flymake-show-buffer-diagnostics) (flymake-goto-next-error))
     :post (close-flymake-diagnostics)
     :hint nil)
    "
^Navigation^      ^Actions^
--------------------------------
_n_: Next         _f_: Quickfix
_N_: Previous     _c_: Consult
_q_: Quit         ^ ^
    "
    ("n" flymake-goto-next-error)
    ("N" flymake-goto-prev-error)
    ("f" eglot-code-action-quickfix)
    ("c" (progn (close-flymake-diagnostics) (consult-flymake)) :color blue)
    ("q" nil :color blue)
  )

  ;; TODO: maybe make q exit multiple cursors and remove region for all packages
  (defhydra hydra-region
    (:hint nil)
    "
^Region^           ^Cursors (%s(symbol-name haf/multicursor-package))^
--------------------------------------------------
^_+_/_C-w_^: ^Expand^  ^_n_/_C-d_^: ^Mark next^
^_-_:^ ^Contract^        ^_N_^: ^Unmark previous^
^_q_:^ ^Quit^            ^_t_^: ^Toggle package^
    "
    ("+" er/expand-region)
    ("C-w" er/expand-region)
    ("-" er/contract-region)
    ("n" haf/add-next-multicursor)
    ("C-d" haf/add-next-multicursor)
    ("N" haf/remove-previous-multicursor)
    ("t" haf/toggle-multicursor-package)
    ("q" nil :color blue)
  )
)

;; TODO: Once there is more than one flymake diagnostics buffer, this won't be able to complete a name (try-completion only finds a prefix) and thus won't close the window
(defun close-flymake-diagnostics ()
  "Close the window on flymake diagnostics"
  (interactive)
  (quit-windows-on (try-completion "*Flymake diagnostics for" (mapcar #'buffer-name (buffer-list)))))

;; TODO: Try to make it a :pre with (call-interactively)
(defun expand-region-and-hydra ()
  (interactive)
  (er/expand-region 1)
  (hydra-region/body)
)
