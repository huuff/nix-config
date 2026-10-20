{ ... }:
{
  sops = {
    defaultSopsFile = ../secrets.yaml;
    secrets = {
      iwdNetworks = { };
      # one ssid per line; mullvad lockdown is relaxed only on these
      trustedNetworks = { };
    };

    age = {
      keyFile = "/var/lib/sops-nix/key.txt";
      generateKey = true;
    };
  };
}
