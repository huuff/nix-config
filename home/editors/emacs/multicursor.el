;; I couldn't decide on a multi-cursor package 
;; (because both multiple-cursors and evil-mc are suboptimal)
;; so I just made a configuration that works for both and made
;; it easy to switch


;; TODO: There is some file that manages which commands are repeated for every cursor...
;; I should inline it here, or edit it here so I can reuse
;; that config for other computers
;; TODO: A doesn't append a cursor and edits to the end of the selection...
;; multiple-cursors
;; ==================
;; the package that looks the most like an emacs native, but it's buggy with evil
(use-package multiple-cursors)

;; evil-mc
;; ==================
;; very powerful for many operations, but it doesn't feel region-oriented since
;; switching to it when a region is selected removes it and creates a single cursor
(use-package evil-mc
  :after evil
  :custom
  ;; remove cursors when pressing C-g
  (evil-mc-undo-cursors-on-keyboard-quit t)
  :config
  (global-evil-mc-mode 1))

;; TODO: Apply some config from a github issue to stop ignoring case
;; TODO: Maybe try to run match and next twice the first time? Since when starting from visual, it just marks
;; the same word as currently marked
;; evil-multiedit
;; ==================
;; contrary to evil-mc it keeps your region, but it lacks evil-mc's power
(use-package evil-multiedit
  :custom
  (evil-multiedit-follow-matches t))

(defvar haf/multicursor-package 'evil-multiedit
  "The package to be used for multiple cursors, either 'evil-mc, 'evil-multiedit or 'multiple-cursors")

;; TODO: Use an array and iterate through it with a modulo length
(defun haf/toggle-multicursor-package ()
  "Toggles the current multi-cursor package"
  (interactive)
  ;; TODO: Can I use pcase instead?
  (cond
   ((eq haf/multicursor-package 'evil-multiedit) (setq haf/multicursor-package 'evil-mc))
   ((eq haf/multicursor-package 'evil-mc) (setq haf/multicursor-package 'multiple-cursors))
   ((eq haf/multicursor-package 'multiple-cursors) (setq haf/multicursor-package 'evil-multiedit))))

(defun haf/add-next-multicursor ()
  "Creates a multi-cursor for the selected region using the package in 'haf/multicursor-package'"
  (interactive)
  ;; TODO: Can I use pcase instead?
  (cond
   ((eq haf/multicursor-package 'multiple-cursors) (call-interactively #'mc/mark-next-like-this))
   ((eq haf/multicursor-package 'evil-mc) (call-interactively #'evil-mc-make-and-goto-next-match))
   ((eq haf/multicursor-package 'evil-multiedit) (call-interactively 'evil-multiedit-match-and-next))))

(defun haf/remove-previous-multicursor ()
  "Removes the last created multi-cursor using the package in 'haf/multicursor-package'"
  (interactive)
  ;; TODO: Can I use pcase instead?
  (cond
   ((eq haf/multicursor-package 'multiple-cursors) (call-interactively #'mc/unmark-next-like-this))
   ((eq haf/multicursor-package 'evil-mc) (call-interactively #'evil-mc-undo-last-added-cursor))
   ;; TODO: Maybe try to remove this option in the hydra?
   ((eq haf/multicursor-package 'evil-multiedit) (message "Not available in evil-multiedit"))))
