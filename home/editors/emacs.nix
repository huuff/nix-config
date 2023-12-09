{ pkgs, ... }:

{
  programs.emacs = {
    enable = true;

    extraPackages = epkgs: [
      epkgs.evil # vim keybindings emulation
      epkgs.use-package # easy package config
      epkgs.bind-key # key binding, apparently needed by use-package?
      epkgs.dracula-theme # theme
      # TODO: Put the command to install icons
      epkgs.all-the-icons # icon pack
      epkgs.treemacs
    ];

    extraConfig = ''
      ;; (use-package)
      (eval-when-compile
        (require 'use-package))

      (load-theme 'dracula t)

      ;; (bind-key) necessary for use-package
      (use-package bind-key
        :ensure t
        :config
        (add-to-list 'same-window-buffer-names "*Personal Keybindings*")
      )

      (use-package treemacs
        :ensure t
        :defer t
        :bind
        (:map global-map
              ("M-0"       . treemacs-select-window)
              ("C-x t 1"   . treemacs-delete-other-windows)
              ("C-x t t"   . treemacs)
              ("C-x t d"   . treemacs-select-directory)
              ("C-x t B"   . treemacs-bookmark)
              ("C-x t C-t" . treemacs-find-file)
              ("C-x t M-t" . treemacs-find-tag))
      )


      ;; (all-the-icons) necessary for neotree
      (use-package all-the-icons
        :if (display-graphic-p))

      ;; Setup evil
      (require 'evil)
      (evil-mode 1)
    '';
  };
}
