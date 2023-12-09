{ pkgs, ... }:

{
  programs.emacs = {
    enable = true;

    extraPackages = epkgs: [
      epkgs.evil # vim keybindings emulation
      epkgs.use-package # easy package config
      epkgs.doom-themes # all the themes from doom emacs
      epkgs.neotree # left-side file explorer
      epkgs.bind-key # key binding, apparently needed by use-package?
      epkgs.all-the-icons # icon pack, needed for neotree
    ];

    extraConfig = ''
      ;; (use-package)
      (eval-when-compile
        (require 'use-package))

      ;; (bind-key) necessary for use-package
      (use-package bind-key
        :ensure t
        :config
        (add-to-list 'same-window-buffer-names "*Personal Keybindings*")
      )

      ;; (all-the-icons) necessary for neotree
      (use-package all-the-icons
        :if (display-graphic-p))

      ;; (neotree)
      (use-package neotree
        :ensure t
        :bind (("<f2>" . neotree-toggle))
      )

      ;; (doom-themes)
      (use-package doom-themes
        :ensure t
        :config
        ;; Global settings (defaults)
        (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
              doom-themes-enable-italic t) ; if nil, italics is universally disabled
        (load-theme 'doom-one t)

        ;; Enable flashing mode-line on errors
        (doom-themes-visual-bell-config)
        ;; Enable custom neotree theme (all-the-icons must be installed!)
        (doom-themes-neotree-config)
        ;; or for treemacs users
        (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
        (doom-themes-treemacs-config)
        ;; Corrects (and improves) org-mode's native fontification.
        (doom-themes-org-config)
      )

      ;; Setup evil
      (require 'evil)
      (evil-mode 1)
    '';
  };
}
