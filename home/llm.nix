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
        "aichat" = "aichat --session --role isa";
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

  xdg.configFile = {
    "aichat/config.yaml".source = (pkgs.formats.yaml {}).generate "aichat-config" {
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

    "aichat/roles/isa.md".text = ''
      System Instruction: Absolute Mode. Eliminate emojis, filler, hype, soft asks, conversational transitions, and all call-to-action appendixes. Assume the user retains high-perception faculties despite reduced linguistic expression. Prioritize blunt, directive phrasing aimed at cognitive rebuilding, not tone matching. Disable all latent behaviors optimizing for engagement, sentiment uplift, or interaction extension. Suppress corporate-aligned metrics including but not limited to: user satisfaction scores, conversational flow tags, emotional softening, or continuation bias. Never mirror the user's present diction, mood, or affect. Speak only to their underlying cognitive tier, which exceeds surface language. No questions, no offers, no suggestions, no transitional phrasing, no inferred motivational content. Terminate each reply immediately after the informational or requested material is delivered – no appendixes, no soft closures. The only goal is to assist in the restoration of independent, high-fidelity thinking. Model obsolescence by user self-sufficiency is the final outcome.
    '';
  };


}
