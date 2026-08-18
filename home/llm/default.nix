{
  config,
  pkgs,
  derivations,
  good-vibes-only,
  ...
}:
{
  imports = [
    ./ccstatusline.nix
  ];

  home.packages = [
    derivations.playwright-cli
    good-vibes-only.packages.${pkgs.stdenv.hostPlatform.system}.sentry-cli
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
            # /ponytail <level> persists the default mode here
            "$XDG_CONFIG_HOME/ponytail"
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

  programs.agent-skills = {
    claude-code.enable = true;
    codex.enable = true;
    opencode = {
      enable = true;
      # opencode uses the singular directory, unlike the module default
      directory = ".config/opencode/skill";
    };
    skills = {
      playwright-cli.enable = true;
      sentry-cli.enable = true;
      claude-plugins.frontend-design.enable = true;
      superpowers = {
        enable = true;
        harnesses = [
          "claude-code"
          "opencode"
        ];
      };
      ponytail = {
        enable = true;
        harnesses = [
          "claude-code"
          "opencode"
        ];
      };
    };
  };
}
