;; compilation-mode customizations

;; TODO: I'm setting these configs into a list so I can have everything in a single place.
;; Then, I'd use find-compilation-config to find the one I want and then I'd use it to run commands and set the regexp alist
;; Maybe a plist is better?
(setq haf/compilation-configs '((:marker-file "Cargo.toml" :build-command "cargo build" :error-regexp '("--> \\(.*?\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))
                                (:marker-file "pnpm-lock.yaml" :build-command "pnpm check")))

(defun haf/find-compilation-config ()
  (let ((default-directory (project-root (project-current))))
    (seq-find (lambda (cfg) (file-exists-p (expand-file-name (cadr (memq :marker-file cfg)) default-directory))) haf/compilation-configs)))

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

