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
          # don't want it to see my secrets
          "sops *" = "deny";

          # scary
          "terraform destroy *" = "deny";
          "tf destoy *" = "deny";
          "terraform apply *" = "deny";
          "tf apply *" = "deny";
        };
      };
      command = {
        q = {
          description = "answer a question";
          agent = "plan";
          template = ''
            Answer the question. Consider the question isolated from the current directory
            or repository unless some file is referenced. Don't explore. Don't edit anyting.
            Try to provide sources and examples to your claims. The question is: $ARGUMENTS
          '';
        };
        solve-issue = {
          description = "solve a github issue";
          agent = "plan";
          template = ''
            Solve the GitHub issue. If you get an issue argument, fetch it
            with gh CLI. Otherwise, try to get it from the current branch name.
            The issue is: "$ARGUMENTS"
          '';
        };
        check-review = {
          description = "check whether a PR review has been addressed";
          agent = "plan";
          template = ''
            Check my previous review of a PR. See if my previous comments have been addressed and
            resolved. Actually check the code changes that address them, not only whether they've been
            marked as resolved. If they've been addressed, briefly describe how (changes in the code).
            If I tell you a PR number, check that PR. Otherwise, try to find it from the current branch name.
            I'll tell you whether to check all previous reviews ("all") or just the latest one ("last").
            If I don' tell you anything, assume it's "all".
            The PR is: "$2"
            Check all/last review: "$1"
          '';
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
      - Don't write obvious comments, comments that explain what is clearly visible from the code,
        or comments that reflect our current conversation. Use comments sparingly, to explain
        code that's complex, or to explain **why** some code exists, if it isn't obvious.

      # Debug summary
      When you've solved a complex issue that required multi-step debugging, provide a summary of
      steps you've taken to detect the source of the issue and to solve it. Don't be too verbose, I'll
      ask more information if I need it. Assume average knowledge of the technologies involved, don't
      be too in-depth nor too beginner friendly.

      # Writing good Rust
      - Whenever you use a function that uses `format_args!`, try to inline arguments inside the string, rather than
        separate. That is, do: `println!("operation succeeded: {operation}")` instead of `println!("operation succeeded: {}", operation)`.
        Do this whenever you can, unless at least one of the arguments can't be inlined, then don't inline any.
      - Try to use OOP style and favor the usage of methods over free functions.

      # Finding the current PR or issue
      When I mention "the PR/issue" or "the current pr/issue" without specifying a number, try to get it from the current
      branch name using the github CLI.

      # Use nix for programs
      It's unlikely that you'll have all the software you need available, but the system is NixOS so you 
      can use the nix command to get anything you want
    '';
    skills = {
      skill-creator = ./skills/skill-creator;
    };
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
