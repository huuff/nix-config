{ pkgs, config, ... }: {

  sops.secrets.openrouterApiKey = {};

  home.packages = [
    # good for scripts with LLM interaction
    (pkgs.python3.withPackages(ps: [ps.llm ps.llm-openrouter]))
    pkgs.claude-code
    pkgs.aichat
  ];

  programs = {
    
    zsh = {
      enable = true;

      shellAliases = {
        # automatically start a new session
        "aichat" = "aichat --session";
      };

      envExtra = ''
        if [[ -f ${config.sops.secrets.openrouterApiKey.path} ]]; then
          # needed for aichat
          export OPENROUTER_API_KEY="$(cat ${config.sops.secrets.openrouterApiKey.path})"
          # needed for llm
          export OPENROUTER_KEY="$(cat ${config.sops.secrets.openrouterApiKey.path})"
        fi
      '';
    };
  };


}
