{ config, ... }: {
  sops.secrets = {
    sshMatchBlocks = {};
  };

  programs.ssh = {
    enable = true;
    extraConfig = ''
      PubkeyAcceptedKeyTypes +ssh-rsa

      Include ${config.sops.secrets.sshMatchBlocks.path}
    '';
  };

}
