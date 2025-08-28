{ ... }:
{
  sops = {
    defaultSopsFile = ../secrets.yaml;
    secrets = {
      wpaSupplicantConf = {};
    };

    age = {
      keyFile = "/var/lib/sops-nix/key.txt";
      generateKey = true;
    };
  };
}
