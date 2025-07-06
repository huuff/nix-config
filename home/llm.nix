{ pkgs, config, ... }: {

  sops.secrets = {
    openrouterApiKey = {};
  };

  home.packages = [
    (pkgs.python3.withPackages(ps: [ps.llm ps.llm-openrouter]))
  ];

  programs = {
    aider = {
      enable = true;
      package = pkgs.aider-chat;
      settings = {
        autoCommits = false;
      };
    };
    
    zsh = {
      enable = true;

      envExtra = ''
        if [[ -f ${config.sops.secrets.openrouterApiKey.path} ]]; then
          # needed for aider
          export OPENROUTER_API_KEY="$(cat ${config.sops.secrets.openrouterApiKey.path})"
          # needed for llm
          export OPENROUTER_KEY="$(cat ${config.sops.secrets.openrouterApiKey.path})"
        fi
      '';
    };
  };


}
