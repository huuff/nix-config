{ pkgs, ... }:

{
  programs.emacs = {
    enable = true;

    extraPackages = epkgs: [
      epkgs.dracula-theme # theme
      epkgs.evil # vim keybindings emulation
    ];

    extraConfig = ''
      ;; Set theme
      (load-theme 'dracula t)

      ;; Setup evil
      (require 'evil)
      (evil-mode 1)
    '';
  };
}
