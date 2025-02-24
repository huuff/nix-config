;; Variables I consider safe in `.dir-locals.el`

;; Formatters I allow in `.dir-locals`
;; An LLM wrote this btw
(put 'apheleia-mode-alist 'safe-local-variable 
     (lambda (val)
       ;; I generally have monorepos where I have some leptos app where I want to use `leptosfmt`
       (let ((safe-formatters '(leptosfmt)))
         (cl-every (lambda (mode-entry)
                     (cl-every (lambda (fmt) (memq fmt safe-formatters))
                               (cdr mode-entry)))
                   val))))
