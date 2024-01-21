(defun delay-fun (f &optional delay)
  "Run some function after a delay, for initializing stuff only after everything else has initialized"
  (run-with-idle-timer (or delay 1) nil f))
