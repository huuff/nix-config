;; TABS CONFIG
;; ==================
;; My config for the topmost tabs for selecting between open buffers. I switch between
;; centaur-tabs and the builtin tab-line with 'haf/tabs-package'


;; TODO: Try to use :general instead of :bind

(defvar haf/tabs-package 'tab-line
  "The package to be used for tabs, either 'centaur' or 'tab-line")

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

;; TODO: Try to make tabs a bit taller
;; TODO: Put some mark in the current active tab?
;; TODO: A mark on modified tabs?
(use-package tab-line
  :ensure nil ;; already included in emacs
  :if (eq haf/tabs-package 'tab-line)
  :init
  (global-tab-line-mode)
  :config

  ;; group buffers that start with * as temporary, and anything in a project
  ;; under the project name
  (defun haf/tab-line-group-by-project (buffer)
    "Use the project.el name for the buffer group"
    (if (or (s-prefix-p "*" (buffer-name buffer))
            (s-prefix-p " *" (buffer-name buffer)))
      "temporary"
      (with-current-buffer buffer
        (let ((prj (project-current)))
          (when prj
            (project-name prj))))))

  ;; sort buffers in a group
  ;; it's pretty important because otherwise the current one is always the first
  ;; which breaks pre  (defun my-buffer-name-sort (a b)
  (setq tab-line-tabs-buffer-group-sort-function #'(lambda (buf1 buf2)
                                                    (string< (buffer-name buf1)
                                                            (buffer-name buf2))))

  ;; do not show the tab-line for these modes
  (setq tab-line-exclude-modes '(
                                  help-mode 
                                  compilation-mode
                                  rustic-compilation-mode
                                ))

  ;; do not go into other groups after last tab, go back
  ;; to the first one
  (setq tab-line-switch-cycling t)

  ;; show tabs according to groups
  ;; the groups are decided by tab-line-tabs-buffer-group-function
  (setq tab-line-tabs-function 'tab-line-tabs-buffer-groups)

  ;; group by project.el project name
  (setq tab-line-tabs-buffer-group-function #'haf/tab-line-group-by-project)

  ;; switching tabs like in vim
  ;; TODO: Try to also configure switching to a specific tab, like: 5 gt goes to the fifth tab
  :general
  (:states 'normal
   "g t" 'tab-line-switch-to-next-tab 
   "g T" 'tab-line-switch-to-prev-tab 
  )
)
