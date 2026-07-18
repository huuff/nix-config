{ pkgs, ... }:
{
  # Glyph donor for the codepoint map below; never used as a text font.
  home.packages = [ pkgs.nerd-fonts.iosevka ];

  programs.ghostty = {
    enable = true;
    settings = {
      # Motivation: I don't think I ever select stuff in a terminal
      # with a different motivation than to copy stuff. So it makes sense
      # that it's saved to clipboard
      copy-on-select = "clipboard";

      # Claude Code's spinner dingbats don't exist in FiraCode (or almost any
      # coding font), so they'd render from whatever fontconfig falls back to
      # (DejaVu). Iosevka draws them bold, uniform and well-centered.
      font-codepoint-map = "U+2722,U+2733,U+2736,U+273B,U+273D=Iosevka Nerd Font";
    };
  };
}
