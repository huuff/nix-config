;; compilation-mode customizations

;; TODO: also set compile-command in case I just want to run it instead of the wrappers?
;; TODO: Also interpret warnings, etc.
(require 'compile)

;; fancy-compilation
;; =====================
;; interprets ANSI escape sequences in compilation mode so it should:
;; * show colors
;; * allow showing single-line auto-updating progress bars to work
;; XXX: Please note that this doesn't actually work for some cases, and it's missing both
;; colors and progress bars
(use-package fancy-compilation
  :demand t
  :config
  (fancy-compilation-mode 1))

(define-compilation-mode rust-compilation-mode "Rust"
  "Rust compilation mode"
  (setq-local compilation-error-regexp-alist '(("--> \\(.*?\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))))

(define-compilation-mode svelte-compilation-mode "Svelte"
  "Rust compilation mode"
  (setq-local compilation-error-regexp-alist '(("\\(.*?/.*?\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))))

(setq haf/compilation-configs (list '(:dominating-file "Cargo.toml"
                                                       :build-command "cargo build"
                                                       :test-command "cargo test"
                                                       :lint-command "cargo clippy"
                                                       :mode rust-compilation-mode)
                                    '(:dominating-file "pnpm-lock.yaml"
                                                       :build-command "pnpm check"
                                                       :test-command "pnpm test -- run"
                                                       :lint-command "pnpx eslint . --format unix"
                                                       :mode svelte-compilation-mode)))

(defun haf/compilation-config-applies (cfg directory)
  "Checks whether a given compilation config applies to the given directory"
  (file-exists-p (expand-file-name (plist-get cfg :dominating-file) directory)))

(defun haf/find-compilation-config (directory)
  (let ((default-directory (project-root (project-current))))
    (seq-find (lambda (cfg) (haf/compilation-config-applies cfg default-directory)) haf/compilation-configs)))

(defun haf/compile-action (action)
  (let* ((default-directory (project-root (project-current)))
         (cfg (haf/find-compilation-config default-directory)))
    (when cfg (compilation-start (plist-get cfg (intern (format ":%s-command" action))) (plist-get cfg :mode)))))

(defun haf/compile-project ()
  (interactive)
  (haf/compile-action "build"))

(defun haf/run-project-tests ()
  (interactive)
  (haf/compile-action "test"))

(defun haf/lint-project ()
  (interactive)
  (haf/compile-action "lint"))

