{ config, pkgs, ... }:
{
  config = {
    networking.wireless.iwd = {
      enable = true;
      settings = {
        General = {
          EnableNetworkConfiguration = true;
        };
        Network = {
          NameResolvingService = "systemd";
        };
      };
    };

    # iwd uses one file per network, so I can create each file with nix,
    # but that requires me to list all my ssids here in plaintext.
    # Instead, I just put them all into a single yaml property in my
    # secrets.yaml, and use this weird trick to create the files
    systemd.services.iwd-secrets = {
      description = "Deploy iwd network secrets";
      wantedBy = [ "multi-user.target" ];
      before = [ "iwd.service" ];
      requiredBy = [ "iwd.service" ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };

      script = ''
        mkdir -p /var/lib/iwd

        ${pkgs.gawk}/bin/awk '
          /^### / {
            if (file) close(file)
            file = "/var/lib/iwd/" substr($0, 5)
            next
          }
          file { print > file }
        ' ${config.sops.secrets.iwdNetworks.path}

        chmod 600 /var/lib/iwd/*.psk 2>/dev/null || true
      '';
    };
  };
}
