(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

(defconst user-init-dir
          (cond ((boundp 'user-emacs-directory)
                 user-emacs-directory)
                ((boundp 'user-init-directory)
                 user-init-directory)
                (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "neuron.el")
(load-user-file "projectile.el")

(setq inhibit-startup-screen t)

(use-package doom-themes
             :config
             ;; Global settings (defaults)
             (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
                   doom-themes-enable-italic t) ; if nil, italics is universally disabled
             (load-theme 'doom-dracula t)

             ;; Enable flashing mode-line on errors
             (doom-themes-visual-bell-config)

             ;; Enable custom neotree theme (all-the-icons must be installed!)
             (doom-themes-neotree-config)
             ;; or for treemacs users
             (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
             (doom-themes-treemacs-config)

             ;; Corrects (and improves) org-mode's native fontification.
             (doom-themes-org-config)

             )

(use-package all-the-icons)

(use-package evil
             :config
             (evil-mode 1)
             (evil-set-initial-state 'help-mode 'emacs)
             (evil-set-initial-state 'treemacs-mode 'emacs)
             (evil-set-initial-state 'geben-mode 'emacs)
             )


(use-package magit)


(use-package counsel
             :config
             (counsel-mode)
             )

(use-package ivy
             :config
             (ivy-mode)
             )

(use-package company
             :config
             (global-company-mode)
             )

(use-package prescient)
(use-package ivy-prescient
             :config
             (ivy-prescient-mode)
             )
(use-package company-prescient
             :config
             (company-prescient-mode)
             )

(use-package flx
             :config
             (setq ivy-re-builders-alist
                   '((t . ivy--regex-fuzzy)))
             )

(use-package which-key
             :config
             (which-key-mode)
             )

; better line wrapping
(global-visual-line-mode t)

; show line numbers
(display-line-numbers-mode)



; cuter font
(add-to-list 'default-frame-alist '(font . "Fira Code" ))
(set-face-attribute 'default t :font "Fira Code" )

; math in markdown mode
(use-package markdown-mode
             :init          
             (setq markdown-enable-math t)
             )

(defun close-and-kill-next-pane ()
  "If there are multiple windows, then close the other pane and kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
    (delete-window)))

(use-package texfrag)

; I literally don't seem to manage to get it to work any other way
(defun texfrag-twice () 
  "Call texfrag-document twice so the latex is correctly generated"
  (interactive)
  (texfrag-document)
  (sleep-for 1)
  (texfrag-document)
  (sleep-for 1)
  (close-and-kill-next-pane)
  )

(use-package doom-modeline
             :ensure t
             :init (doom-modeline-mode 1))

(use-package rainbow-mode
             :config (rainbow-mode))

(use-package centaur-tabs
             :demand
             :config
             (centaur-tabs-mode t)
             (setq centaur-tabs-label-fixed-length 18)
             :bind
             ("C-<prior>" . centaur-tabs-backward)
             ("C-<next>" . centaur-tabs-forward))

(use-package evil-nerd-commenter)
(use-package evil-leader
             :config
             (evil-leader/set-key
               "ci" 'evilnc-comment-or-uncomment-lines
               "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
               "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
               "cc" 'evilnc-copy-and-comment-lines
               "cp" 'evilnc-comment-or-uncomment-paragraphs
               "cr" 'comment-or-uncomment-region
               "cv" 'evilnc-toggle-invert-comment-line-by-line
               "."  'evilnc-copy-and-comment-operator
               "\\" 'evilnc-comment-operator
               )
             )

(use-package php-mode
:mode ("\\.php\\'" . php-mode)
)

(use-package nix-mode
    :mode ("\\.nix\\'" . nix-mode)
    )

(use-package lsp-mode
             :config
             :hook (php-mode . lsp)
             :hook (nix-mode . lsp)
             :hook (sh-mode . lsp)
             )

(use-package dap-mode
             :config
             (dap-mode 1)
             )

(use-package dap-php)

