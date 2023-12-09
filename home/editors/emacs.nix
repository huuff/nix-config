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

      ;; (dracula)
      (load-theme 'dracula t)

      ;; (bind-key) necessary for use-package
      (use-package bind-key
        :ensure t
        :config
        (add-to-list 'same-window-buffer-names "*Personal Keybindings*")
      )

      ;; (treemacs)
      (use-package treemacs
        :ensure t
        :defer t
        :bind
        (:map global-map
              ("C-x t t" . treemacs))
        :hook (emacs-startup . treemacs)
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
