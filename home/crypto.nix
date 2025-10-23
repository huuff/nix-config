{ pkgs, ... }:
{
  home.packages = with pkgs; [
      monero-gui
  ];
}
