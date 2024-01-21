;; TODO: Explain what this beast does

(defvar haf/ran-rustic-dependency-management nil
  "Whether some command last ran a cargo dependency management command")

;; XXX: I don't even know what the smth arg is
(defun haf/advice-set-ran-rustic-dependency-management (smth)
  "Sets 'haf/ran-rustic-dependency-management', used for advising functions that run rustic dependency management"
  (setq haf/ran-rustic-dependency-management t)
)

;; TODO: Maybe a better name that expresses that it unsets
;; TODO: Maybe also check the buffer name
;; TODO: Do not run the delay-fun unless it's actually set!
(defun haf/is-rustic-dependency-management (&optional buffer)
  "Check whether this is a rustic command for dependency management (add or remove dependency), and then unsets it"
  ;(message (concat "run buffer: " (symbol-value buffer)))
  (message "is-rustic-depencency-management buffer: %S" buffer)
  (delay-fun (lambda () (setq haf/ran-rustic-dependency-management nil)))
  haf/ran-rustic-dependency-management
)
