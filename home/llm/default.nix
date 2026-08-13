{
  config,
  pkgs,
  derivations,
  superpowers,
  playwright-cli-src,
  sentry-cli-src,
  claude-plugins-src,
  good-vibes-only,
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
  imports = [
    ./ponytail.nix
    ./ccstatusline.nix
  ];

  home.packages = [
    derivations.playwright-cli
    sentry-cli
  ];

  programs.zsh.envExtra = ''
    # playwright-cli uses this to find the browser
    export PLAYWRIGHT_MCP_EXECUTABLE_PATH="${pkgs.chromium}/bin/chromium"
  '';

  programs.nono = {
    enable = true;
    package = derivations.nono;
    profiles = {
      claude-haf = {
        extends = "claude-code";
        meta.description = "claude-code pack + ccstatusline, sentry, playwright and cargo access";
        filesystem = {
          read = [
            "$XDG_CONFIG_HOME/ccstatusline"
            "$HOME/.cargo"
          ];
          # gitconfig's includeIf for ~/work; git hard-fails on unreadable includes
          read_file = [ "$HOME/.config/sops-nix/secrets/gitWorkConfig" ];
          # base packs only grant write on /tmp, not read
          allow = [
            "/tmp"
            "$HOME/.od"
            "$HOME/.sentry"
            "$HOME/.cache/ms-playwright"
          ];
        };
      };
      codex-haf = {
        extends = "codex";
        meta.description = "codex pack + cargo access";
        filesystem = {
          read = [ "$HOME/.cargo" ];
          # gitconfig's includeIf for ~/work; git hard-fails on unreadable includes
          read_file = [ "$HOME/.config/sops-nix/secrets/gitWorkConfig" ];
          # base packs only grant write on /tmp, not read
          allow = [
            "/tmp"
            "$HOME/.od"
          ];
        };
      };
    };
    wrappers = {
      nono-claude = {
        profile = "claude-haf";
        command = "claude --dangerously-skip-permissions";
        allowGitCommonDir = true;
        extraFlags = [ "--allow-cwd" ];
      };
      nono-codex = {
        profile = "codex-haf";
        # nono is the sandbox, so codex's own approvals/sandbox get bypassed
        command = "codex --dangerously-bypass-approvals-and-sandbox";
        allowGitCommonDir = true;
        extraFlags = [ "--allow-cwd" ];
      };
    };
  };

  # Home Manager generates config.toml for the shared Open Design MCP server.
  programs.codex = {
    enable = true;
    package = good-vibes-only.packages.${pkgs.stdenv.hostPlatform.system}.codex-trust-state;
    enableMcpIntegration = true;
    context = ''
      # Use nix for programs
      It's unlikely that you'll have all the software you need available, but the system is NixOS so you
      can use the nix command to get anything you want. Some ways to do it:
      - Run a one-off command: `nix run github:NixOS/nixpkgs/nixos-unstable#<pkg> -- <args>`
      - Get a shell with tools on PATH: `nix shell github:NixOS/nixpkgs/nixos-unstable#<pkg1> github:NixOS/nixpkgs/nixos-unstable#<pkg2> -c <command>`
      - Search for a package: `nix search github:NixOS/nixpkgs/nixos-unstable <term>`
      Use this direct flake reference instead of the `nixpkgs#<pkg>` shorthand. Nix's indirect registry
      lookup opens `/`, which is denied inside the nono sandbox.
    '';
  };

  programs.claude-code = {
    enable = true;
    enableMcpIntegration = true;
    package = derivations.claude-code;
    plugins = [ superpowers ];
    settings = {
      model = "claude-fable-5";
      permissions.deny = [
        "Read(**/.env)"
        "Read(**/.env.local)"
      ];
      skipDangerousModePermissionPrompt = true;
      skipAutoPermissionPrompt = true;
      # flicker-free alt-screen renderer with virtualized scrollback
      tui = "fullscreen";
      # refreshInterval because Claude Code only re-renders on events, so the
      # branch would go stale when it changes in another terminal.
      # type/command come from programs.ccstatusline.
      statusLine.refreshInterval = 5;
    };
    # command is not a store path so the repo devshell can resolve it
    lspServers.rust-analyzer = {
      command = "rust-analyzer";
      extensionToLanguage.".rs" = "rust";
    };
    context = ''
      # Use nix for programs
      It's unlikely that you'll have all the software you need available, but the system is NixOS so you
      can use the nix command to get anything you want. Some ways to do it:
      - Run a one-off command: `nix run github:NixOS/nixpkgs/nixos-unstable#<pkg> -- <args>`
      - Get a shell with tools on PATH: `nix shell github:NixOS/nixpkgs/nixos-unstable#<pkg1> github:NixOS/nixpkgs/nixos-unstable#<pkg2> -c <command>`
      - Search for a package: `nix search github:NixOS/nixpkgs/nixos-unstable <term>`
      Use this direct flake reference instead of the `nixpkgs#<pkg>` shorthand. Nix's indirect registry
      lookup opens `/`, which is denied inside the nono sandbox.
    '';
  };

  programs.mcp = {
    enable = true;
    servers.open-design = {
      command = pkgs.lib.getExe config.services.open-design.package;
      args = [
        "mcp"
        "--daemon-url"
        "http://127.0.0.1:${toString config.services.open-design.port}"
      ];
      env.OD_DATA_DIR = toString config.services.open-design.dataDir;
    };
  };

  # programs.opencode.skills and programs.claude-code.skills don't handle
  # string-interpolated store paths correctly (lib.isPath returns false), so
  # we use xdg.configFile / home.file directly.
  xdg.configFile."opencode/skill/playwright-cli" = {
    source = "${playwright-cli-src}/skills/playwright-cli";
    recursive = true;
  };
  home.file = {
    ".claude/skills/playwright-cli" = {
      source = "${playwright-cli-src}/skills/playwright-cli";
      recursive = true;
    };

    # Anthropic's official frontend-design skill, vendored straight from the
    # plugins repo instead of going through the /plugin marketplace.
    ".claude/skills/frontend-design" = {
      source = "${claude-plugins-src}/plugins/frontend-design/skills/frontend-design";
      recursive = true;
    };
  };

  programs.agent-skills.claude-code.enable = true;

  # The .cursor/skills/sentry-cli path contains a symlink that breaks with
  # recursive xdg.configFile, so we point to the real location instead.
  xdg.configFile."opencode/skill/sentry-cli" = {
    source = "${sentry-cli-src}/plugins/sentry-cli/skills/sentry-cli";
    recursive = true;
  };

}
