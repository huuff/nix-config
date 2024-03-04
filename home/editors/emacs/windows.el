
(setq haf/popup-buffers '((:mode help-mode :size 0.3 :align below :select t)
                          (:mode helpful-mode :size 0.3 :align below :select t)
                          (:mode compilation-mode :size 0.4 :align right :select nil)
                          ;; TODO: tab-line still not disappearing for messages?
                          (:mode messages-buffer-mode :size 0.3 :align below :select nil)
                          ;; TODO: Not exactly working... why?
                          (:mode inferior-emacs-lisp-mode :size 0.3 :align below :select t)
                          ;; TODO: Also not exactly working like ielm!
                          (:mode eshell-mode :size 0.3 :align below :select t)
                          (:mode magit-status-mode :size 0.4 :align below :select t)))

(defun haf/popup-modes ()
  (mapcar #'(lambda (it) (plist-get it :mode)) haf/popup-buffers))

;; shackle
;; =====================
;; allows configuring in what how each buffer will show
(use-package shackle
  :init
  (shackle-mode)
  :config
  (setq shackle-rules (mapcar #'cdr haf/popup-buffers)))

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
