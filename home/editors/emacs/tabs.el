;; TABS CONFIG
;; I use centaur tabs but (TODO) I'd love to be able to achieve a similar config
;; using the built-in tab-line mode.
;; In the future, I hope to set up some config to chose between the two

(use-package centaur-tabs
  :init
    (setq centaur-tabs-enable-key-bindings t)
  :demand
  :config
    (centaur-tabs-mode t)
    ;; make tabs take full width
    (centaur-tabs-headline-match)
    ;; show icons
    (setq centaur-tabs-set-icons t)
    ;; gray-out unactive tab
    (setq centaur-tabs-gray-out-icons 'buffer)
    ;; left active tab indicator
    (setq centaur-tabs-set-bar 'left)
    ;; show whether tab is modified
    (setq centaur-tabs-set-modified-marker t)
    ;; TODO: I don't know how but I'm separating centaur tabs by project
    ;; which drives me a little crazy when switching between files in different projects
    ;; try to disable this, and only hide unwanted tabs (such as temporary buffers)

    ;; do not allow crossing tab groups by changing tab 
    ;; (prevents going to weird hidden buffers when going
    ;; to next tab on the last one)
    (setq centaur-tabs-cycle-scope 'tabs)

    ;; TODO: actually, I think I should set centaur-tabs-excluded-prefixes!
    ;; TODO: Also, it's now showing on which-key too
    ;; hide in some buffers
    (defun centaur-tabs-hide-tab (x)
      "Do no to show buffer X in tabs."
      (let ((name (format "%s" x)))
        (or
         ;; current window is not dedicated window.
         (window-dedicated-p (selected-window))

         ;; buffer does not start with an asterisk (temporary buffer)
         (string-prefix-p "*" name)
     )))

  :bind
  ;; vim-like change tabg with `g t` and `g T`
  ;; TODO: But can I make 2 gt for going to the second tab for example?
  (:map evil-normal-state-map
    ("g t" . centaur-tabs-forward)
    ("g T" . centaur-tabs-backward))
)

