;; compilation-mode customizations

;; TODO: Maybe I'd need to put these configs into alists instead of into cond/cl-case
;; TODO: maybe a separate function to check instead of compile
;; TODO: a separate function for running tests
;; TODO: also set compile-command in case I just want to run it instead of the wrappers?
;; TODO: Compile regexps
(defun haf/compile-project ()
  "Run a compile command specific for this kind of project"
  (interactive)
  (let* ((default-directory (project-root (project-current)))
         (project-type (haf/get-project-type default-directory)))
    (cl-case project-type
      ('cargo (let ((compilation-error-regexp-alist '(("--> \\(.*?\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)))) (compile "cargo build")))
      ('pnpm (compile "pnpm check"))
      ('npm (compile "npm run build")))))

(defun haf/get-project-type (directory)
  "Detects the type of project for a given directory"
  (cond ((file-exists-p (expand-file-name "Cargo.toml" directory)) 'cargo)
        ((file-exists-p (expand-file-name "pnpm-lock.yaml" directory)) 'pnpm)
        ((file-exists-p (expand-file-name "package-lock.json" directory)) 'npm)))

;; (setq my/cargo-build-error-regexp
;;       "--> \\(.*?\\):\\([0-9]+\\):\\([0-9]+\\)"
;;       )

;; (add-to-list 'compilation-error-regexp-alist-alist
;;              '(my/cargo-build-error-regexp 1 2 3))

;; (add-to-list 'compilation-error-regexp-alist 'my/cargo-build-error-regexp)

;; (setq compilation-error-regexp-alist (eval (car (get 'compilation-error-regexp-alist 'standard-value))))

;; (setq compilation-error-regexp-alist
;;       '(("--> \\(.*?\\):\\([0-9]+\\):\\([0-9]+\\)"
;;          1 2 3)))
