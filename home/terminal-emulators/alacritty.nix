{
  programs.alacritty = {
    enable = true;
    settings = {
      # Motivation: I don't think I ever select stuff in a terminal
      # with a different motivation than to copy stuff. So it makes sense
      # that it's saved to clipboard
      selection.save_to_clipboard = true;
      keyboard.bindings = [
        # shift + return for newline, especially useful for claude code
        {
          key = "Return";
          mods = "Shift";
          chars = "\\n";
        }
      ];
    };
  };
}
