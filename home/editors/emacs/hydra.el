(use-package hydra
  :config
  ;; TODO: Allow switching to project diagnostics
  ;; TODO: Different keybindings for errors and warnings/notes (prefix goes to these for flymake-goto-«next or previous»-error)
  ;; TODO: This barely works, go into a rust file, delete a long import section and try to fix them one by one, you'll find that it's broken all around
  (defhydra hydra-flymake
    (:pre (progn (flymake-show-buffer-diagnostics) (flymake-goto-next-error))
     :post (haf/close-flymake-diagnostics)
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
    (:hint nil
    )
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
(defun haf/close-flymake-diagnostics ()
  "Close the window on flymake diagnostics"
  (interactive)
  (quit-windows-on (try-completion "*Flymake diagnostics for" (mapcar #'buffer-name (buffer-list)))))


(defun haf/expand-and-start-region-hydra ()
  "Expands region and runs the hydra 'hydra-region'."
  (interactive)
  (er/expand-region 1)
  (hydra-region/body)
)

(defun haf/next-cursor-and-start-region-hydra ()
  "Adds a multicursor and runs the hydra 'hydra-region'"
  (interactive)
  (haf/add-next-multicursor)
  (hydra-region/body)
 )
