{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.gnupg ];

  programs.gnupg.agent = {
    enable = true;
    # necessary for collecting entropy for generating keys
    pinentryPackage = pkgs.pinentry-curses;
    settings = {
      # key lasts in cache for 30 mins even if unused
      default-cache-ttl = 1800;
      # key cache renews when used, up to a max of 8 hours
      max-cache-ttl = 28800;
    };
  };
}
