;; compilation-mode customizations

(require 'compile)

;; TODO: maybe a separate function to check instead of compile
;; TODO: a separate function for running tests
;; TODO: also set compile-command in case I just want to run it instead of the wrappers?
(define-compilation-mode rust-compilation-mode "Rust"
  "Rust compilation mode"
  (setq-local compilation-error-regexp-alist '(("--> \\(.*?\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))))

(setq haf/compilation-configs (list '(:dominating-file "Cargo.toml" :build-command "cargo build" :mode rust-compilation-mode)
                                    '(:dominating-file "pnpm-lock.yaml" :build-command "pnpm check")
                                    '(:dominating-file "package-lock.json" :build-command "npm run build")))

(defun haf/compilation-config-applies (cfg directory)
  "Checks whether a given compilation config applies to the given directory"
  (file-exists-p (expand-file-name (plist-get cfg :dominating-file) directory)))

(defun haf/find-compilation-config (directory)
  (let ((default-directory (project-root (project-current))))
    (seq-find (lambda (cfg) (haf/compilation-config-applies cfg default-directory)) haf/compilation-configs)))

(defun haf/compile-project ()
  (interactive)
  (let* ((default-directory (project-root (project-current)))
         (cfg (haf/find-compilation-config default-directory)))
    (when cfg (compilation-start (plist-get cfg :build-command) (plist-get cfg :mode)))))


