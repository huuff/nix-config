{ pkgs, ... }:
{

  home.packages = with pkgs; [
    kubectl
    krew
  ];

  home.sessionPath = [ "$HOME/.krew/bin" ];
}
