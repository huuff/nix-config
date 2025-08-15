{ pkgs, config, ... }: {

  sops.secrets = {
    openrouterApiKey = {};
  };

  home.packages = [
    (pkgs.python3.withPackages(ps: [ps.llm ps.llm-openrouter]))
    pkgs.claude-code
  ];

  programs = {
    aider = {
      enable = true;
      package = pkgs.aider-chat;
      settings = {
        auto-commits = false;
        alias = [ "gemini:openrouter/google/gemini-2.5-pro" ];
        model = "gemini";
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
