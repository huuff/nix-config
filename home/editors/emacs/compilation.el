;; compilation-mode customizations

;; TODO: maybe a separate function to check instead of compile
;; TODO: a separate function for running tests
;; TODO: also set compile-command in case I just want to run it instead of the wrappers?
;; TODO: Compile regexps
(defun haf/compile-project ()
  "Run a compile command specific for this kind of project"
  (interactive)
  (let ((default-directory (project-root (project-current))))
    ;; TODO: this broke and I don't know when
    (cond ((eq major-mode 'rust-ts-mode) (compile "cargo build"))
          ;; TODO: More types of build systems
          ((eq major-mode 'svelte-mode) (compile "pnpm check"))
          ((eq major-mode 'typescript-ts-mode) (compile "pnpm check")))))
