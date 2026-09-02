{ config, ... }:
{

  programs.ssh = {
    enable = true;
    extraConfig = ''
      PubkeyAcceptedKeyTypes +ssh-rsa
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

  # Sandboxed apps (bwrap/flatpak) see the store symlink as owned by nobody; ssh rejects it
  home.file.".ssh/config" = {
    target = ".ssh/config_source";
    onChange = ''
      install --mode 400 ~/.ssh/config_source ~/.ssh/config
    '';
  };

}
