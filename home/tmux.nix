{ pkgs, myOverlays, ... }:
{
  nixpkgs.overlays = [ myOverlays.tmuxPlugins ];

  programs.tmux = {
    enable = true;
    extraConfig = ''
        # easy to remember keybindings for splits
        bind | split-window -h -c "#{pane_current_path}"
        bind - split-window -v -c "#{pane_current_path}"
        bind c new-window -c "#{pane_current_path}"  

        # clear history with C-k
        bind -n C-k clear-history
    '';
    plugins = with pkgs; [
      tmuxPlugins.yank
      {
        plugin = tmuxPlugins.better-mouse-mode;
        extraConfig = ''
          set-option -g mouse on
          set-option -g @emulate-scroll-for-no-mouse-alternate-buffer on
        '';
      }
    ];
  };

}
