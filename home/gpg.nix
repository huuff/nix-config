{
  good-vibes-only,
  pkgs,
  ...
}:
{
  home.packages = [
    good-vibes-only.packages.${pkgs.stdenv.hostPlatform.system}.keyloader
  ];

  programs.gpg.enable = true;

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 3600;
    maxCacheTtl = 3600;
    pinentry.package = pkgs.pinentry-curses;

    # Let keyloader preset passphrases in the agent cache.
    extraConfig = "allow-preset-passphrase";
  };
}
