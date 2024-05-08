(defun haf/delay-fun (f &optional delay)
  "Run some function after a delay, for initializing stuff only after everything else has initialized"
  (run-with-idle-timer (or delay 1) nil f))

(defun haf/indent-region (&rest ignored)
  "Indents selected region, I use it as an advice for functions that move lines"
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))
