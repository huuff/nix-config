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
    enableDefaultConfig = false;

    matchBlocks."*" = {
      forwardAgent = false;
      serverAliveInterval = 0;
      serverAliveCountMax = 3;
      compression = false;
      extraOptions = {
        AddKeysToAgent = "no";
        HashKnownHosts = "no";
        UserKnownHostsFile = "~/.ssh/known_hosts";
        ControlMaster = "no";
        ControlPath = "~/.ssh/master-%r@%n:%p";
        ControlPersist = "no";
      };
    };
  };

}
