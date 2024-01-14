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
