{
  good-vibes-only,
  pkgs,
  ...
}:
{
  home.packages = [
    good-vibes-only.packages.${pkgs.stdenv.hostPlatform.system}.default
  ];

  # keyloader needs allow-preset-passphrase to preset key passphrases into gpg-agent
  services.gpg-agent = {
    enable = true;
    extraConfig = "allow-preset-passphrase";
    maxCacheTtl = 86400; # preset entries expire with this TTL
  };
}
