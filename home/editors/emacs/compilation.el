;; compilation-mode customizations

(require 'compile)

(define-compilation-mode rust-compilation-mode "Rust"
  "Rust compilation mode"
  (setq-local compilation-error-regexp-alist '(("--> \\(.*?\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))))

(setq haf/compilation-configs (list '(:dominating-file "Cargo.toml" :build-command "cargo build" :mode rust-compilation-mode)
                                    '(:dominating-file "pnpm-lock.yaml" :build-command "pnpm check")
                                    '(:dominating-file "package-lock.json" :build-command "npm run build")))

(defun haf/compilation-config-applies (cfg directory)
  "Checks whether a given compilation config applies to the given directory"
  (file-exists-p (expand-file-name (plist-get cfg :dominating-file) directory)))

(defun haf/find-compilation-config ()
  (let ((default-directory (project-root (project-current))))
    (seq-find (lambda (cfg) (haf/compilation-config-applies cfg default-directory)) haf/compilation-configs)))

;; TODO: maybe a separate function to check instead of compile
;; TODO: a separate function for running tests
;; TODO: also set compile-command in case I just want to run it instead of the wrappers?
(defun haf/compile-project ()
  "Run a compile command specific for this kind of project"
  (interactive)
  (let* ((default-directory (project-root (project-current)))
         (project-type (haf/get-project-type default-directory)))
    (cl-case project-type
      ('cargo (compilation-start "cargo build" 'rust-compilation-mode))
      ('pnpm (compile "pnpm check"))
      ('npm (compile "npm run build")))))

(defun haf/get-project-type (directory)
  "Detects the type of project for a given directory"
  (cond ((file-exists-p (expand-file-name "Cargo.toml" directory)) 'cargo)
        ((file-exists-p (expand-file-name "pnpm-lock.yaml" directory)) 'pnpm)
        ((file-exists-p (expand-file-name "package-lock.json" directory)) 'npm)))


