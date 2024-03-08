;; TODO: Maybe change the close, prev/next icons? they look ugly
;; TODO: Maybe there's too much space around tab names?

(use-package tab-line
  :ensure nil ;; already included in emacs
  :init
  (global-tab-line-mode)
  :custom-face
  ;; I need a different font family to make it italic when modified
  ;; because my main font (Fira Code) has no italic version
  (tab-line-tab ((t (:family "Hack" :height 1.05))))
  (tab-line-tab-current ((t (:inherit tab-line-tab :weight bold))))
  (tab-line-tab-modified ((t (:inherit tab-line-tab :slant italic))))
  :preface
  (defun haf/tab-line-group-by-project (buffer)
    "Split buffers into appropriate groups"
    (with-current-buffer buffer
      (let ((bufname (buffer-name buffer)))
        ;; group buffers that start with an asterisk under "temporary"
        (cond ((or (s-prefix-p "*" bufname)
                   (s-prefix-p " *" bufname)
                   ;; also put this weird buffer there since it's regularly popping up
                   (s-prefix-p "Treemacs Update Single File" bufname)) "temporary")
              ;; group paths under /nix/store under the "external" grou
              ;; if I don't do this, project-current will run for these and CPU usage
              ;; goes through the roof
              ((s-prefix-p "/nix/store" (buffer-file-name buffer)) "external")
              ((s-prefix-p "magit" bufname) "magit")
              ((eq major-mode 'dired-mode) "dired")
              ((eq major-mode 'dired-sidebar-mode) "dired")
              ;; otherwise, group by current project.el project
              (t (if-let ((prj (project-current))) (project-name prj) "other"))))))

  ;; some utility functions
  (defun haf/tab-line-tab-is-selected (tab)
    "Returns whether a given tab is currently selected"
    (cdr (assoc 'selected tab)))
  (defun haf/tab-line-tab-is-group (tab)
    "Returns whether a given tab is a group tab"
    (cdr (assoc 'group-tab tab)))

  ;; fun to close all non-active tabs
  (defun haf/tab-line-close-other-tabs ()
    "Close all tabs in current group that aren't active"
    (interactive)
    (let ((other-tabs (cl-remove-if #'(lambda (tab) (or (haf/tab-line-tab-is-selected tab)
                                                        (haf/tab-line-tab-is-group tab))) (tab-line-tabs-buffer-groups))))
      (dolist (tab other-tabs) (funcall (cdr (assoc 'close tab))))))

  ;; I skillfully copy-pasted most of this function off tab-line-tabs-buffer-groups
  (defun haf/tab-line-tabs-buffer-groups ()
    "Returns a list of buffers in the current tab group"
    (let* ((window-parameter (window-parameter nil 'tab-line-group))
           (group-name (tab-line-tabs-buffer-group-name (current-buffer)))
           (group (prog1 (or window-parameter group-name "All")
                    (when (equal window-parameter group-name)
                      (set-window-parameter nil 'tab-line-group nil))))
           (buffers
            (seq-filter (lambda (b)
                          (equal (tab-line-tabs-buffer-group-name b) group))
                        (funcall tab-line-tabs-buffer-list-function)))
           (sorted-buffers (if (functionp tab-line-tabs-buffer-group-sort-function)
                               (seq-sort tab-line-tabs-buffer-group-sort-function
                                         buffers)
                             buffers)))
      sorted-buffers))

  (defun haf/tab-line-tab-name (buffer &optional _buffers)
    "Make space in tab line names for all the junk I'm gonna put into them"
    (format " %s " (buffer-name buffer)))

  ;; I also skillfully copied most of this off
  ;; https://github.com/zhenhua-wang/emacs.d/blob/ca6056dbb3c03997cc72431b90490606652756b1/module/zw-tab-line.el#L96
  (defun haf/tab-line-tab-name-format (orig-fun &rest args)
    "Appends the tab index and an icon to the name of each tab"
    (let* ((tab-string (apply orig-fun args))
           (buffer-name (string-trim (string-replace tab-line-close-button "" tab-string))))
      (if-let ((buffer (get-buffer buffer-name)))
          (let* ((tab-pos (1+ (seq-position (haf/tab-line-tabs-buffer-groups) buffer)))
                 (selected-p (eq buffer (window-buffer)))
                 (icon (if (buffer-file-name buffer)
                           (nerd-icons-icon-for-file (buffer-file-name buffer))
                         (with-current-buffer buffer
                           (nerd-icons-icon-for-mode major-mode))))
                 (icon-face-raw (get-text-property 0 'face icon))
                 (icon-face (if selected-p
                                (if (mode-line-window-selected-p)
                                    (list :inherit icon-face-raw
                                          :height (face-attribute 'tab-line-tab-current :height)
                                          :background (face-background 'tab-line-tab-current)
                                          :overline (face-attribute 'tab-line-tab-current :overline))
                                  ;; TODO: Some icons (such as nix) won't show nicely with this
                                  (list :inherit icon-face-raw
                                        :height (face-attribute 'tab-line-tab :height)
                                        :background (face-background 'tab-line-tab)
                                        :overline (face-attribute 'tab-line-tab :overline)))
                              'tab-line-tab-inactive))
                 (space-face (if selected-p
                                 (if (mode-line-window-selected-p)
                                     'tab-line-tab-current
                                   'tab-line-tab)
                               'tab-line-tab-inactive))
                 (space (propertize " " 'face space-face
                                    'keymap tab-line-tab-map
                                    'mouse-face 'tab-line-highlight))
                 (tab-index-str (propertize (number-to-string tab-pos) 'face (get-text-property 0 'face tab-string))))
            (concat space
                    tab-index-str
                    space
                    (propertize icon 'face icon-face
                                'keymap tab-line-tab-map
                                'mouse-face 'tab-line-highlight)
                    tab-string
                    space))
        tab-string)))
  

  (defun haf/switch-to-tab-index (count)
    "Switch to a tab by its number in current group"
    (interactive "p")
    (let ((tabs (cl-remove-if #'haf/tab-line-tab-is-group (tab-line-tabs-buffer-groups))))
      (if-let ((target-tab (nth (1- count) tabs)))
          (switch-to-buffer (cdr (assoc 'buffer target-tab)))
        (error (format "There is no tab at index %d" count)))))

  (evil-define-command haf/tab-line-goto-tab (count)
    :repeat nil
    (interactive "<c>")
    (if (not count)
        (tab-line-switch-to-next-tab)
      (haf/switch-to-tab-index count)))

  :config
  ;; only use special face functions for modified tabs. I removed styles for special tabs because I didn't like nor need them
  (setq tab-line-tab-face-functions '(tab-line-tab-face-modified))


  ;; sort buffers in a group
  ;; it's pretty important because otherwise the current one is always the first
  ;; which breaks switching to next/prev
  (setq tab-line-tabs-buffer-group-sort-function #'(lambda (buf1 buf2)
                                                     (string< (buffer-name buf1)
                                                              (buffer-name buf2))))

  ;; do not show the tab-line for these modes
  (setq tab-line-exclude-modes (append '(dashboard-mode
                                         magit-diff-mode
                                         dired-mode
                                         dired-sidebar-mode)
                                       (haf/popup-modes)))

  ;; do not go into other groups after last tab, go back
  ;; to the first one
  (setq tab-line-switch-cycling t)

  ;; show tabs according to groups
  ;; the groups are decided by tab-line-tabs-buffer-group-function
  (setq tab-line-tabs-function 'tab-line-tabs-buffer-groups)

  ;; group by project.el project name
  (setq tab-line-tabs-buffer-group-function #'haf/tab-line-group-by-project)


  (setq tab-line-tab-name-function #'haf/tab-line-tab-name)

  (advice-add 'tab-line-tab-name-format-default :around
              #'haf/tab-line-tab-name-format)

  ;; switching tabs like in vim
  :general
  (:states 'normal
           "g t" 'haf/tab-line-goto-tab
           "g T" 'tab-line-switch-to-prev-tab))
