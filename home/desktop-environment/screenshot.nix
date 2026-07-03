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
    # select a region, annotate it in swappy, then copy the result to the clipboard
    (pkgs.writeShellApplication {
      name = "paintscrot";
      runtimeInputs = with pkgs; [
        hyprshot
        swappy
        wl-clipboard
      ];
      text = "hyprshot --mode region --raw | swappy -f - -o - | wl-copy --type image/png";
    })
  ];
}
