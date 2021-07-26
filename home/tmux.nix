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
      {
        plugin = tmuxPlugins.yank;
        extraConfig = 
        # For robustness, I can easily change it here if I use wayland, or MacOS
        let
          copyToPrimary = "${pkgs.xsel}/bin/xsel -i";
          copyToClipboard = "${pkgs.xsel}/bin/xsel -i --clipboard";
          pasteFromPrimary = "${pkgs.xsel}/bin/xsel -o";
        in
        ''
          set -g @yank_action 'copy-pipe-no-clear' # Do not exit copy mode when selecting text

          # Copy to word to primary on double click, line on triple click
          bind -T copy-mode    DoubleClick1Pane select-pane \; send -X select-word \; send -X copy-pipe-no-clear "${copyToPrimary}"
          bind -T copy-mode-vi DoubleClick1Pane select-pane \; send -X select-word \; send -X copy-pipe-no-clear "${copyToPrimary}"
          bind -n DoubleClick1Pane select-pane \; copy-mode -M \; send -X select-word \; send -X copy-pipe-no-clear "${copyToPrimary}"
          bind -T copy-mode    TripleClick1Pane select-pane \; send -X select-line \; send -X copy-pipe-no-clear "${copyToPrimary}"
          bind -T copy-mode-vi TripleClick1Pane select-pane \; send -X select-line \; send -X copy-pipe-no-clear "${copyToPrimary}"
          bind -n TripleClick1Pane select-pane \; copy-mode -M \; send -X select-line \; send -X copy-pipe-no-clear "${copyToPrimary}"

          # Copy to clipboard on Ctrl + C
          bind -T copy-mode    C-c send -X copy-pipe-no-clear "${copyToClipboard}"
          bind -T copy-mode-vi C-c send -X copy-pipe-no-clear "${copyToClipboard}"

          # Which means I need a different command for exiting copy mode, say Ctrl + X
          bind -T copy-mode    C-x send -X cancel
          bind -T copy-mode-vi C-x send -X cancel

          # Paste on middle click
          bind -n MouseDown2Pane run "tmux set-buffer -b primary_selection \"$(${pasteFromPrimary})\"; tmux paste-buffer -b primary_selection; tmux delete-buffer -b primary_selection"
        '';
      }
      {
        plugin = tmuxPlugins.better-mouse-mode;
        extraConfig = ''
          set-option -g mouse on
          set-option -g @emulate-scroll-for-no-mouse-alternate-buffer on # Use mouse for scrolling in vim, less
        '';
      }
    ];
  };

}
