
;; shackle
;; =====================
;; allows configuring in what how each buffer will show
(use-package shackle
  :init
  (shackle-mode)
  :config
  (setq
   shackle-rules '((help-mode :size 0.3 :align below :select t)
                   (helpful-mode :size 0.3 :align below :select t))))

;; popper
;; =====================
;; mark buffers as "pop-up", making them easy to dismiss, toggle and cycle through
(use-package popper
  :ensure t
  :init
  (setq popper-reference-buffers '("\\*Messages\\*"
                                   "Output\\*$"
                                   "\\*Async Shell Command\\*"
                                   help-mode
                                   helpful-mode
                                   compilation-mode))
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
