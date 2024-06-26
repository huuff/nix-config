
;; TODO: Some alignment for calc-mode and also remove tabs from it
;; TODO: Add the flymake window here too
(setq haf/popup-buffers '((:mode help-mode :size 0.3 :align below :select t :popup t)
                          (:mode helpful-mode :size 0.3 :align below :select t :popup t)
                          (:mode compilation-mode :size 0.4 :align right :select nil :popup t)
                          (:mode messages-buffer-mode :size 0.3 :align below :select nil :popup t)
                          (:mode inferior-emacs-lisp-mode :size 0.3 :align below :select t :buffer-name "*ielm*" :popup t)
                          (:mode eshell-mode :size 0.3 :align below :select t :buffer-name "\\*.*?eshell\\*" :regexp t :popup t)
                          (:mode magit-status-mode :size 0.4 :align below :select t :popup t)
                          (:mode literate-calc-mode :buffer-name "*literate-calc*" :size 0.3 :align below :select t :popup t)))

(defun haf/popup-modes ()
  (mapcar #'(lambda (it) (plist-get it :mode)) haf/popup-buffers))

(defun haf/popup-buffer-entry-by-bufname (entry)
  "Transforms a popup-buffers entry so that the bufname is the first element"
  (let* ((bufname (plist-get entry :buffer-name))
         (entry-without-mode (cddr entry)))
    (cons bufname entry-without-mode)))

;; Some modes won't work with shackle's ways because they are initiated after the buffer is created.
;; this function will transform each haf/popup-buffers entry into one that has the buffer name at the beginning
;; so it fits shackle's declarations for detecting buffers by name.
;; example: (:mode eshell-mode :size 0.3 :align below :select t :buffer-name "*eshell*")
;; transforms into: ("*eshell*" :size 0.3 :align below :select t :buffer-name "*eshell*")
(defun haf/popup-buffers-by-bufname ()
  "Transforms entries so that the buffer name is the first element"
  (let ((bufname-entries (seq-filter #'(lambda (entry) (plist-get entry :buffer-name)) haf/popup-buffers)))
    (mapcar #'haf/popup-buffer-entry-by-bufname bufname-entries)))

;; shackle
;; =====================
;; allows configuring in what how each buffer will show
(use-package shackle
  :init
  (shackle-mode)
  :custom
  (shackle-rules (append
                  (mapcar #'cdr haf/popup-buffers)
                  (haf/popup-buffers-by-bufname))))

;; popper
;; =====================
;; mark buffers as "pop-up", making them easy to dismiss, toggle and cycle through
(use-package popper
  :ensure t
  :init
  (setq popper-reference-buffers (haf/popup-modes))
  (popper-mode +1)
  ;; enables echo area hints
  (popper-echo-mode +1)
  ;; do not do any display control, let shackle do it
  (setq popper-display-control nil))

;; ace-window
;; =====================
;; quickly switch between windows like when navigating around a buffer with avy
(use-package ace-window
  :commands ace-window)
