{ vibe-keyloader, ... }:
{
  # The module's default package is pkgs.keyloader, which comes from the overlay
  nixpkgs.overlays = [ vibe-keyloader.overlays.default ];

  programs.keyloader.enable = true;

  # keyloader needs allow-preset-passphrase to preset key passphrases into gpg-agent
  services.gpg-agent = {
    enable = true;
    extraConfig = "allow-preset-passphrase";
    maxCacheTtl = 86400; # preset entries expire with this TTL
  };
}
