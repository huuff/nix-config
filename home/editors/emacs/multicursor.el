;; I couldn't decide on a multi-cursor package 
;; (because both multiple-cursors and evil-mc are suboptimal)
;; so I just made a configuration that works for both and made
;; it easy to switch


;; TODO: There is some file that manages which commands are repeated for every cursor...
;; I should inline it here, or edit it here so I can reuse
;; that config for other computers
;; TODO: A doesn't append a cursor and edits to the end of the selection...
(use-package multiple-cursors)

;; TODO: A doesn't append a cursor and edits to the end of the selection...
;; TODO: Please make it easier to exit the multicursor mode
(use-package evil-mc
  :after evil
  :config (global-evil-mc-mode 1)
)

(defvar haf/multicursor-package 'multiple-cursors
  "The package to be used for multiple cursors, either 'evil-mc or 'multiple-cursors")

(defun haf/toggle-multicursor-package ()
  "Toggles the current multi-cursor package"
  (interactive)
  (cond
    ((eq haf/multicursor-package 'multiple-cursors) (setq haf/multicursor-package 'evil-mc))
    ((eq haf/multicursor-package 'evil-mc) (setq haf/multicursor-package 'multiple-cursors))))

(defun haf/add-next-multicursor ()
  "Creates a multi-cursor for the selected region using the package in 'haf/multicursor-package'"
  (interactive)
  (cond
    ((eq haf/multicursor-package 'multiple-cursors) (call-interactively #'mc/mark-next-like-this))
    ((eq haf/multicursor-package 'evil-mc) (call-interactively #'evil-mc-make-and-goto-next-match))))

(defun haf/remove-previous-multicursor ()
  "Removes the last created multi-cursor using the package in 'haf/multicursor-package'"
  (interactive)
  (cond
    ((eq haf/multicursor-package 'multiple-cursors) (call-interactively #'mc/unmark-next-like-this))
    ((eq haf/multicursor-package 'evil-mc) (call-interactively #'evil-mc-undo-last-added-cursor))))
