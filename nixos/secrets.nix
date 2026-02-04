{ ... }:
{
  sops = {
    defaultSopsFile = ../secrets.yaml;
    secrets = {
      iwdNetworks = { };
    };

    age = {
      keyFile = "/var/lib/sops-nix/key.txt";
      generateKey = true;
    };
  };
}
