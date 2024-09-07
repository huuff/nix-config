;; compilation-mode customizations

;; TODO: also set compile-command in case I just want to run it instead of the wrappers?
;; TODO: Also interpret warnings and infos
;; TODO: My rust regex fails, specially for tests
(use-package compile
  :ensure nil ;; already included in emacs
  :preface
  (defun haf/compilation-config-applies (cfg directory)
    "Checks whether a given compilation config applies to the given directory"
    (file-exists-p (expand-file-name (plist-get cfg :dominating-file) directory)))

  (defun haf/find-compilation-config (directory)
    (let ((default-directory (project-root (project-current))))
      (seq-find (lambda (cfg) (haf/compilation-config-applies cfg default-directory)) haf/compilation-configs)))

  (defun haf/compile-action (action)
    (let* ((default-directory (project-root (project-current)))
           (cfg (haf/find-compilation-config default-directory)))
      (when cfg (compile (plist-get cfg (intern (format ":%s-command" action)))))))

  (defun haf/compile-project ()
    (interactive)
    (haf/compile-action "build"))

  (defun haf/run-project-tests ()
    (interactive)
    (haf/compile-action "test"))

  (defun haf/lint-project ()
    (interactive)
    (haf/compile-action "lint"))
  :config
  ;; TODO: Some types of cargo errors are not highlighted (test, maybe?) note them here to fix them
  (setq compilation-error-regexp-alist '(("--> \\(.*?\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3) ;; cargo build
                                         ("\\(^/.*?\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))) ;; svelte check 


  (setq haf/compilation-configs (list '(:dominating-file "Cargo.toml"
                                                         :build-command "cargo build --all-features"
                                                         :test-command "cargo test --all-features"
                                                         :lint-command "cargo clippy")
                                      '(:dominating-file "pnpm-lock.yaml"
                                                         :build-command "pnpm check"
                                                         :test-command "pnpm test -- run"
                                                         ;; TODO: This is kinda terrible because I install eslint every time
                                                         :lint-command "pnpx eslint . --format unix")))
  ;; always enable next-error-follow-minor-mode for compilation mode
  :hook (compilation-mode . next-error-follow-minor-mode))

;; fancy-compilation
;; =====================
;; interprets ANSI escape sequences in compilation mode so it should:
;; * show colors
;; * allow showing single-line auto-updating progress bars to work
(use-package fancy-compilation
  :demand t
  :config
  (fancy-compilation-mode 1))

