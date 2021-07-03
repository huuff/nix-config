(use-package projectile
             :config
             (projectile-mode +1)
             (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
             )

(use-package counsel-projectile
             :config
             (counsel-projectile-mode)
             )


