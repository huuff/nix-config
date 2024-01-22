;; TODO: Explain what this beast does

(defvar haf/ran-rustic-dependency-management nil
  "Whether some command last ran a cargo dependency management command")

;; XXX: I don't even know what the smth arg is
(defun haf/advice-set-ran-rustic-dependency-management (smth)
  "Sets 'haf/ran-rustic-dependency-management', used for advising functions that run rustic dependency management"
  (setq haf/ran-rustic-dependency-management t)
  (delay-fun (lambda () (setq haf/ran-rustic-dependency-management nil)))
)

;; TODO: Maybe a better name that expresses that it unsets
;; TODO: Maybe also check the buffer name
(defun haf/is-rustic-dependency-management (&optional buffer)
  "Check whether this is a rustic command for dependency management (add or remove dependency), and then unsets it"
  haf/ran-rustic-dependency-management
)
