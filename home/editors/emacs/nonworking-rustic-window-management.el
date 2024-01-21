
(defvar haf/ran-rustic-dependency-management nil
  "Whether some command last ran a cargo dependency management command")

;; XXX: I don't even know what the smth arg is
(defun haf/advice-set-ran-rustic-dependency-management (smth)
  "Sets 'haf/ran-rustic-dependency-management', used for advising functions that run rustic dependency management"
  (setq haf/ran-rustic-dependency-management t)
)

(defun haf/is-rustic-dependency-management (&optional buffer)
  "Check whether this is a rustic command for dependency management (add or remove dependency), and then unsets it"
  ;(message (concat "run buffer: " (symbol-value buffer)))
  (message "is-rustic-depencency-management buffer: %S" buffer)
  (let ((res haf/ran-rustic-dependency-management))
    (message "res value: %S" res)
    ;(setq haf/ran-rustic-dependency-management nil)
    (message "res value: %S" res)
    res
  )
)
