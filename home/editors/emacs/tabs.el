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

;; TODO: Remove the | from the selected tab (bold is enough)
;; TODO: Try to use italics for modified tabs names (but my font (Fira Code) has no italics)
;; TODO: Try to make tabs a bit taller
(use-package tab-line
  :ensure nil ;; already included in emacs
  :if (eq haf/tabs-package 'tab-line)
  :init
  (global-tab-line-mode)
  :custom-face
  (tab-line-tab-current ((t (:inherit tab-line-tab :weight bold))))
  :config
  ;; only use special face functions for modified tabs. I removed styles for special tabs because I didn't like nor need them
  (setq tab-line-tab-face-functions '(
                                        tab-line-tab-face-modified))

  ;; group buffers criteria
  (defun haf/tab-line-group-by-project (buffer)
    "Use the project.el name for the buffer group"
    ;; group buffers that start with an asterisk under "temporary"
    (cond ((or (s-prefix-p "*" (buffer-name buffer))
               (s-prefix-p " *" (buffer-name buffer))
               ;; also put this weird buffer there since it's regularly popping up
               (s-prefix-p "Treemacs Update Single File" (buffer-name buffer))) "temporary")
          ;; group paths under /nix/store under the "external" grou
          ;; if I don't do this, project-current will run for these and CPU usage
          ;; goes through the roof
          ((s-prefix-p "/nix/store" (buffer-file-name buffer)) "external")
          ;; otherwise, group by current project.el project
          (t (with-current-buffer buffer
              (let ((prj (project-current)))
               (if prj (project-name prj) "other"))))))

  ;; fine-tune the name of the tabs
  (defun haf/tab-line-tab-name (buffer)
    "Stylize the tab name"
    ;; append * to modified buffers
    (format "%s%s%s"
      ;; prepend a vertical bar to current-buffer
      (if (eq (current-buffer) buffer) "|" "")
      ;; buffer name in the middle
      (buffer-name buffer)
      ;; asterisk at the end for modified buffers
      (if (buffer-modified-p buffer) "*" "")))

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
                                  dashboard-mode
                                ))

  ;; do not go into other groups after last tab, go back
  ;; to the first one
  (setq tab-line-switch-cycling t)

  ;; show tabs according to groups
  ;; the groups are decided by tab-line-tabs-buffer-group-function
  (setq tab-line-tabs-function 'tab-line-tabs-buffer-groups)

  ;; group by project.el project name
  (setq tab-line-tabs-buffer-group-function #'haf/tab-line-group-by-project)

  ;; custom tab name
  (setq tab-line-tab-name-function #'haf/tab-line-tab-name)

  ;; fun to close all non-active tabs
  (defun haf/tab-line-close-other-tabs ()
    "Close all tabs in current group that aren't active"
    (interactive)
    (let ((other-tabs (cl-remove-if (lambda (tab) (or
                                                    (cdr (assoc 'selected tab))
                                                    (cdr (assoc 'group-tab tab)))) (tab-line-tabs-buffer-groups))))
      (dolist (tab other-tabs) (funcall (cdr (assoc 'close tab))))))


  ;; switching tabs like in vim
  ;; TODO: Try to also configure switching to a specific tab, like: 5 gt goes to the fifth tab
  :general
  (:states 'normal
   "g t" 'tab-line-switch-to-next-tab 
   "g T" 'tab-line-switch-to-prev-tab 
  )
)
