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
  :config 
  ;; remove cursors when pressing C-g
  (setq evil-mc-undo-cursors-on-keyboard-quit t)
  (global-evil-mc-mode 1))

;; TODO: Apply some config from a github issue to stop ignoring case
;; TODO: Maybe try to run match and next twice the first time? Since when starting from visual, it just marks
;; the same word as currently marked
;; evil-multiedit
;; ==================
;; contrary to evil-mc it keeps your region, but it lacks evil-mc's power
(use-package evil-multiedit
  :custom (evil-multiedit-follow-matches t "Jump to each next match instead of just marking it"))

(defcustom haf/multicursor-package 'evil-multiedit
  "The package to be used for multiple cursors, either 'evil-mc, 'evil-multiedit or 'multiple-cursors"
  :type '(choice (const :tag "multiple-cursors" multiple-cursors)
                 (const :tag "evil-multiedit" evil-multiedit)
                 (const :tag "evil-mc" evil-mc)))

;; TODO: Use an array and iterate through it with a modulo length
(defun haf/toggle-multicursor-package ()
  "Toggles the current multi-cursor package"
  (interactive)
  (cl-case haf/multicursor-package
    (evil-multiedit (setq haf/multicursor-package 'evil-mc))
    (evil-mc (setq haf/multicursor-package 'multiple-cursors))
    (multiple-cursors (setq haf/multicursor-package 'evil-multiedit))))

(defun haf/add-next-multicursor ()
  "Creates a multi-cursor for the selected region using the package in 'haf/multicursor-package'"
  (interactive)
  (call-interactively (cl-case haf/multicursor-package
                        ('multiple-cursors #'mc/mark-next-like-this)
                        ('evil-mc #'evil-mc-make-and-goto-next-match)
                        ('evil-multiedit #'evil-multiedit-match-and-next))))

(defun haf/remove-previous-multicursor ()
  "Removes the last created multi-cursor using the package in 'haf/multicursor-package'"
  (interactive)
  (cl-case haf/multicursor-package
    (multiple-cursors (call-interactively #'mc/unmark-next-like-this))
    (evil-mc (call-interactively #'evil-mc-undo-last-added-cursor))
    ;; TODO: Maybe try to remove this option in the transient?
    (evil-multiedit (message "Not available in evil-multiedit"))))
