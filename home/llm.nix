{ unstablePkgs, config, lib, ... }: {

  sops.secrets = {
    openrouterApiKey = {};
  };

  programs.aider = {
    enable = true;
    package = unstablePkgs.aider-chat;
    settings = {
      autoCommits = false;
    };
  };

  programs.zsh = {
    enable = true;

    envExtra = ''
      if [[ -f ${config.sops.secrets.openrouterApiKey.path} ]]; then
        export OPENROUTER_API_KEY="$(cat ${config.sops.secrets.openrouterApiKey.path})"
      fi
    '';
  };

}
