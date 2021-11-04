{ pkgs, emacs-overlay, ... }:
{
  nixpkgs.overlays = [ emacs-overlay.overlay ];

  #services.emacs.enable = true;
  programs.emacs = {
    enable = true;
    extraPackages = (epkgs:
    (with epkgs; [
      use-package
      evil
      magit
      projectile
      ivy
      counsel
      flx
      counsel-projectile
      hydra
      neuron-mode
      company
      texfrag
      prescient
      ivy-prescient
      company-prescient
      which-key
      doom-modeline
      rainbow-mode
      centaur-tabs
      evil-nerd-commenter
      evil-leader
      evil-snipe
      php-mode
      nix-mode
      dap-mode
      lsp-mode

      # UI
      treemacs
      treemacs-evil
      treemacs-projectile
      treemacs-all-the-icons
      treemacs-magit
      all-the-icons
      doom-themes   

      # Required for treemacs, not sure it's working
      pkgs.python3
      pkgs.nodePackages.npm
      pkgs.nodejs
    ]));
  };

  home.file = {
    ".emacs.d" = {
    source = ./emacs.d;
    recursive = true;
    };
    };

    xresources.properties = {
    # Set some Emacs GUI properties in the .Xresources file because they are
    # expensive to set during initialization in Emacs lisp. This saves about
    # half a second on startup time. See the following link for more options:
    # https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html#Fonts
    "Emacs.menuBar" = false;
    "Emacs.toolBar" = false;
    "Emacs.verticalScrollBars" = false;
    "Emacs.font" = "";
    };
    }
