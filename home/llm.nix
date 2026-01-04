{
  config,
  pkgs,
  ...
}:
{
  # AICHAT
  sops.secrets.openrouterApiKey = { };

  programs.aichat = {
    enable = true;
    settings = {
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
  };

  xdg.configFile."aichat/roles/isa.md".text = ''
    System Instruction: Absolute Mode. Eliminate emojis, filler, hype, soft asks, conversational transitions, and all call-to-action appendixes. Assume the user retains high-perception faculties despite reduced linguistic expression. Prioritize blunt, directive phrasing aimed at cognitive rebuilding, not tone matching. Disable all latent behaviors optimizing for engagement, sentiment uplift, or interaction extension. Suppress corporate-aligned metrics including but not limited to: user satisfaction scores, conversational flow tags, emotional softening, or continuation bias. Never mirror the user's present diction, mood, or affect. Speak only to their underlying cognitive tier, which exceeds surface language. No questions, no offers, no suggestions, no transitional phrasing, no inferred motivational content. Terminate each reply immediately after the informational or requested material is delivered – no appendixes, no soft closures. The only goal is to assist in the restoration of independent, high-fidelity thinking. Model obsolescence by user self-sufficiency is the final outcome.
  '';

  programs.zsh.shellAliases = {
    "aichat" = "aichat --session --role isa";
  };

  programs.zsh.envExtra = ''
    if [[ -f ${config.sops.secrets.openrouterApiKey.path} ]]; then
      # needed for aichat
      export OPENROUTER_API_KEY="$(cat ${config.sops.secrets.openrouterApiKey.path})"
      # needed for llm
      export OPENROUTER_KEY="$(cat ${config.sops.secrets.openrouterApiKey.path})"
    fi
  '';

  # CLAUDE CODE
  # XXX: This hack is the only way I've found to setup different auth for work and for
  # my personal config. Any other choice is pure hell and doesn't seem like anthropic really
  # wants to support that.
  # I can't use the home manager module for claude code because it won't allow me to have two
  # different dirs
  home.packages = [ pkgs.claude-code ];

  home.file =
    let
      claudeSettings = builtins.toJSON {
        model = "sonnet";
        enabledPlugins = {
          "rust-analyzer-lsp@claude-plugins-official" = true;
        };
        permissions = {
          defaultMode = "plan";
        };
      };
    in
    {
      ".claude/personal/settings.json".text = claudeSettings;
      ".claude/work/settings.json".text = claudeSettings;
    };

  programs.zsh.initExtra = ''
    # Claude Code config switcher based on working directory
    claude() {
      if [[ "$PWD" == "$HOME/work"* ]]; then
        CLAUDE_CONFIG_DIR=~/.claude/work command claude "$@"
      else
        CLAUDE_CONFIG_DIR=~/.claude/personal command claude "$@"
      fi
    }
  '';
}
