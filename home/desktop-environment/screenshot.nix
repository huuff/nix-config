{ pkgs, ... }:
{
  programs.hyprshot = {
    enable = true;
    saveLocation = "$HOME/screenshots";
  };

  home.packages = [
    # select a region and screenshot it directly to the clipboard
    (pkgs.writeShellApplication {
      name = "clipscrot";
      runtimeInputs = with pkgs; [ hyprshot ];
      text = "hyprshot --mode region --clipboard-only";
    })
  ];
}
