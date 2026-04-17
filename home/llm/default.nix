{
  config,
  pkgs,
  derivations,
  playwright-cli-src,
  sentry-cli-src,
  ...
}:
let
  # HACK: Bun single-executable binaries embed JS at the end of the ELF.
  # dontStrip prevents the nix strip phase from removing the embedded code,
  # and we use only patchelf --set-interpreter (not autoPatchelfHook) to
  # avoid rpath changes that shift the embedded code offset.
  sentry-cli = pkgs.stdenv.mkDerivation {
    pname = "sentry-cli";
    version = "0.24.1";
    src = pkgs.fetchurl {
      url = "https://github.com/getsentry/cli/releases/download/0.24.1/sentry-linux-x64";
      hash = "sha256-LkolQnaaGLB2/5kfkrDVwXTQ1MAmwxlYelWPi4q9CGc=";
    };
    dontUnpack = true;
    dontStrip = true;
    nativeBuildInputs = [ pkgs.patchelf ];
    installPhase = ''
      install -Dm755 $src $out/bin/sentry
      patchelf --set-interpreter ${pkgs.glibc}/lib64/ld-linux-x86-64.so.2 $out/bin/sentry
    '';
  };
in
{
  imports = [ ./superpowers.nix ];

  home.packages = [
    derivations.playwright-cli
    sentry-cli
  ];

  programs.zsh.envExtra = ''
    # playwright-cli uses this to find the browser
    export PLAYWRIGHT_MCP_EXECUTABLE_PATH="${pkgs.chromium}/bin/chromium"
  '';

  # OPENCODE
  programs.opencode = {
    enable = true;
    package = derivations.opencode;
    settings = {
      model = "anthropic/claude-opus-4-5-20250929";
      autoupdate = false;
      mcp = { };
      permission = {
        bash = {
          # playwright-cli is safe to auto-allow (browser automation via skill)
          "playwright-cli *" = "allow";

          # sentry-cli is safe to auto-allow (Sentry interaction via skill)
          "sentry *" = "allow";

          # don't want it to see my secrets
          "sops *" = "deny";
          # in case the command has the "sops" string by chance
          "*sops *" = "ask";

          # don't like it committing without my permission
          "git commit *" = "ask";
          "git push *" = "deny";

          # I don't like it reviewing its own work, prefer to check
          # snapshots myself
          "cargo insta *" = "ask";

          # scary
          "*terraform destroy*" = "deny";
          "*terraform *" = "ask";
          "*tofu destroy*" = "deny";
          "*tofu *" = "ask";
          "*tf destroy*" = "deny";
          "*tf *" = "ask";
        };
      };
      agent = {
        careful = {
          description = "Ask before running any command";
          mode = "primary";
          permission = {
            bash."*" = "ask";
            edit = "ask";
          };
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
      - Within a file, put the most important piece of code at the top, and any helper functions
        and utilities at the bottom.
      - Prefer deep functions where steps are fenced by comments instead of decomposing into many
        small shallow functions. Only decompose into functions when a) smaller functions are going
        to be reused in several sites, or b) parts of the main functions are so large and complex 
        that they overshadow the rest of the logic.

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
      - Don't implement `Default` on types that don't have a meaningful, semantic default value. In cases where you need a quick way to
        construct an instance for testing, derive `fake::Dummy` instead.
      - Don't give expressions a type if it can be inferred.
      - Try to never silence warnings, if they're pre-existing and you want to solve them, ask for permission first. If you think they're
        unsolvable, ask for permission to silence them, but always prefer `#[expect]` to `#[allow]`
      - Prefer using `try_collect` and `collect_vec` from `itertools` to using `collect::<Result<_, _>>` and `collect::<Vec<_>>` 

      # Finding the current PR or issue
      When I mention "the PR/issue" or "the current pr/issue" without specifying a number, try to get it from the current
      branch name using the github CLI.

      # Use nix for programs
      It's unlikely that you'll have all the software you need available, but the system is NixOS so you 
      can use the nix command to get anything you want

      # Don't snoop into my secrets
      NEVER run `sops` commands to decrypt my secrets, not directly and not through `nix run` nor anything like that.
      NEVER open my .env files. You're free to know what my secrets are (e.g. reading encrypted sops secrets) but not their actual
      contents.
    '';
    skills = {
      skill-creator = ./skills/skill-creator;
    };
  };

  # programs.opencode.skills doesn't handle string-interpolated store paths
  # correctly (lib.isPath returns false), so we use xdg.configFile directly.
  xdg.configFile."opencode/skill/playwright-cli" = {
    source = "${playwright-cli-src}/skills/playwright-cli";
    recursive = true;
  };

  # The .cursor/skills/sentry-cli path contains a symlink that breaks with
  # recursive xdg.configFile, so we point to the real location instead.
  xdg.configFile."opencode/skill/sentry-cli" = {
    source = "${sentry-cli-src}/plugins/sentry-cli/skills/sentry-cli";
    recursive = true;
  };

}
