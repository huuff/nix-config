{
  programs.ghostty = {
    enable = true;
    settings = {
      # Motivation: I don't think I ever select stuff in a terminal
      # with a different motivation than to copy stuff. So it makes sense
      # that it's saved to clipboard
      copy-on-select = "clipboard";
    };
  };
}
