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

  ;; TODO: maybe make q exit multiple cursors, and maybe also remove the region
  ;; TODO: A doesn't append to the end of the cursor... maybe I could just map A in the hydra to some mc equivalent?
  (defhydra hydra-region
    (:hint nil)
    "
^Region^        ^Cursors^
-------------------------------
_+_: Expand     _n_: Mark next
_-_: Contract   _N_: Unmark previous
_q_: Quit       ^ ^
    "
    ("+" er/expand-region)
    ("-" er/contract-region)
    ("n" mc/mark-next-like-this)
    ("N" mc/unmark-next-like-this)
    ("q" nil :color blue)
  )
)

;; TODO: Once there is more than one flymake diagnostics buffer, this won't be able to complete a name (try-completion only finds a prefix) and thus won't close the window
(defun close-flymake-diagnostics ()
  "Close the window on flymake diagnostics"
  (interactive)
  (quit-windows-on (try-completion "*Flymake diagnostics for" (mapcar #'buffer-name (buffer-list)))))

(defun expand-region-and-hydra ()
  (interactive)
  (er/expand-region 1)
  (hydra-region/body)
)
