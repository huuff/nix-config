{ pkgs, ... }:
{
  
  #services.pcscd.enable = true;
  programs.gnupg.agent = {
    enable = true;
    # necessary for collecting entropy for generating keys
    pinentryPackage = pkgs.pinentry-curses;
  };
}

