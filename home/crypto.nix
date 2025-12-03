{ pkgs, ... }:
{
  home.packages = with pkgs; [
      monero-gui
      ledger-live-desktop 
  ];
}
