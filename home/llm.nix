{ pkgs, lib, config, ... }: {

  sops.secrets.openrouterApiKey = {};

  home.packages = [
    # good for scripts with LLM interaction
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

  xdg.configFile."aichat/config.yaml".source = (pkgs.formats.yaml {}).generate "aichat-config" {
    model = "openrouter:google/gemini-3-pro-preview";
    clients = [
      {
        type = "openai-compatible";
        name = "openrouter";
        api_base = "https://openrouter.ai/api/v1";
        patch = {
          chat_completions = {
            ".*" = {
              body = {
                # remove <think> blocks which are just noise, I'd prefer if aichat had some setting
                # to hide/show it, but this is my quickest solution
                include_reasoning = false;
              };
            };
          };
        };
      }
    ];
    save_session = true;
  };

}
