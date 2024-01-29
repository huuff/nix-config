;; TABS CONFIG
;; I use centaur tabs but (TODO) I'd love to be able to achieve a similar config
;; using the built-in tab-line mode.
;; In the future, I hope to set up some config to chose between the two

;; TODO: Try to use :general instead of :bind

(defvar haf/tabs-package 'tab-line
  "The package to be used for tabs, either 'centaur or 'tab-line")

;; TODO: Split workspaces by some useful criterion rather than just by project.
;; for example, excluding by treemacs workspaces would be great (but that would depend on treemacs)

;; TODO: Maybe make a hydra/transient for tabbing around

(use-package centaur-tabs
  :if (eq haf/tabs-package 'centaur)
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

    ;; do not allow crossing tab groups by changing tab 
    ;; (prevents going to weird hidden buffers when going
    ;; to next tab on the last one)
    (setq centaur-tabs-cycle-scope 'tabs)

    ;; exclude all temporary (*) buffers
    ;; note that some start with a space, like ' *which-key*'
    (setq centaur-tabs-excluded-prefixes '("*" " *"))

  :bind
  ;; vim-like change tabg with `g t` and `g T`
  ;; TODO: But can I make 2 gt for going to the second tab for example?
  (:map evil-normal-state-map
    ("g t" . centaur-tabs-forward)
    ("g T" . centaur-tabs-backward))
)

(use-package tab-line
  :ensure nil ;; already included in emacs
  :if (eq haf/tabs-package 'tab-line)
  :init
  (global-tab-line-mode)
  :config
  ;; do not show the tab-line for these modes
  (setq tab-line-exclude-modes '(
                                  help-mode 
                                  compilation-mode
                                  rustic-compilation-mode
                                )
  )

  ;; do not go into other groups after last tab, go back
  ;; to the first one
  (setq tab-line-switch-cycling t)

  ;; show tabs according to groups
  ;; note that the group isn't actually configured and it's using
  ;; the default (group by major mode) unless I (TODO) set up
  ;; the tab-line-tabs-buffer-groups-function
  (setq tab-line-tabs-function 'tab-line-tabs-buffer-groups)
  :general
  (:states 'normal
   "g t" 'tab-line-switch-to-next-tab 
   "g T" 'tab-line-switch-to-prev-tab 
  )
)
