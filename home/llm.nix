{
  config,
  pkgs,
  derivations,
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

  # OPENCODE
  programs.opencode = {
    enable = true;
    package = derivations.opencode;
    settings = {
      model = "anthropic/claude-opus-4-5-20250929";
      autoupdate = false;
      permission = {
        bash = {
          "sops *" = "deny";
        };
      };
    };
    rules = ''
      # Writing one-off hacks
      When you have to write a hack for a limitation in a library, and
      especially if it's a bug or known issue, try to keep it away from the
      main logic, and clearly marked.

      ## Keeping it away from the main logic
      You could hide it behind a function with a descriptive name and call
      that function, rather than inline the code of the hack within the main logic
      or another function.

      Do this unless it's too inconvenient, or the hack is very short itself.
      If you have any doubts about whether this rule is applicable, ask me.

      ## Marking it
      Use a comment to mark it as a `// HACK`. Explain the reason why it's needed.
      If there's an issue in GitHub, link it in the comment.

      # Writing good code
      - Try not to hardcode stuff. Whenever possible, get it from some single source of truth.
        If not possible, don't extract it to a constant unless it's re-used, instead inline it
        at the use-site.
    '';
  };

  # HACK: OpenCode doesn't support multiple auth profiles yet.
  # This workaround uses different XDG_DATA_HOME dirs based on working directory.
  # https://github.com/anomalyco/opencode/issues/5391#issuecomment-3649792123
  programs.zsh.initExtra = ''
    opencode() {
      if [[ "$PWD" == "$HOME/work"* ]]; then
        XDG_DATA_HOME=~/.local/share/opencode-work command opencode "$@"
      else
        XDG_DATA_HOME=~/.local/share/opencode-personal command opencode "$@"
      fi
    }
  '';
}
