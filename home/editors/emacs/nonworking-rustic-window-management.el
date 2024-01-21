;; XXX, TODO: I was working hard on getting rustic-compilation buffers to open differently depending on whether they were
;; for actual compilation commands like `rustic-cargo-build` or
;; for dependency management commands like `rustic-cargo-add`.
;; In the end, the only "solution" I found is advising all cargo
;; dependency commands so they set some variable and then read that variable to decide how to open a buffer with `shackle.el`.
;; I do this in shackle.el with a :custom function for deciding the target configuration, on which I reset the variable (so not all subsequent commands run as if they were a rustic dependency managenent command).
;; This hasn't worked because apparently the :custom function for shackle is running twice! And only the last result is used.
;; Therefore, if I unset the dependency management variable and run the same command again, the second time it's unset!
;; The only lame idea I have is to set up a timer to unset the variable, so it stays set for enough time to work for the shackle.el :custom function

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
