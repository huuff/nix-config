{ ... }:
{
  programs.starship = {
    enable = true;
    # Default symbols are emoji (with VS16) which ghostty correctly renders 2 cells
    # wide, leaving ugly gaps. This preset swaps them all for single-cell nerd-font
    # glyphs (FiraCode Nerd Font is installed via stylix.fonts.packages).
    presets = [ "nerd-font-symbols" ];
    settings = {
      format = "$all$kubernetes$cmd_duration$line_break$character";
      add_newline = false;
      kubernetes = {
        disabled = false;
        format = "on [⎈ $cluster(\\($namespace\\))](blue) ";
      };
      # ~/.config/gcloud existing is enough to trigger this module, but with no
      # active account/project it renders a bare icon followed by nothing.
      gcloud.disabled = true;
    };

  };
}
