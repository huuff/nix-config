;; hydra
;; =================
;; allows writing special pop-ups to dispatch commands and do operations
;; these produce namespaces where operations can be repeated with single key presses
;; also they look very nice and can explain what they do
(use-package hydra
  :config

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
    ("q" nil :color blue)))

(defun haf/expand-and-start-region-hydra ()
  "Expands region and runs the hydra 'hydra-region'."
  (interactive)
  (er/expand-region 1)
  (hydra-region/body))

(defun haf/next-cursor-and-start-region-hydra ()
  "Adds a multicursor and runs the hydra 'hydra-region'"
  (interactive)
  (haf/add-next-multicursor)
  (hydra-region/body))
