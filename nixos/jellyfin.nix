{ pkgs, lib, ... }:
{
  nixpkgs.config.permittedInsecurePackages = [
    "qtwebengine-5.15.19"
  ];

  programs.firejail = {
    enable = true;
    wrappedBinaries = {
      # jellyfin has some very insecure qt version, my only choice is to
      # add it at the system level and wrap it in firejail so it doesn't steal my
      # bitcoins
      jellyfinmediaplayer = {
        executable = lib.getExe pkgs.jellyfin-media-player;
      };
    };
  };
}
