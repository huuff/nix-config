{ vibe-keyloader, ... }:
{
  # The module's default package is pkgs.keyloader, which comes from the overlay
  nixpkgs.overlays = [ vibe-keyloader.overlays.default ];

  programs.keyloader.enable = true;
}
