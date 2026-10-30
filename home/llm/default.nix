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
    good-vibes-only.packages.${pkgs.stdenv.hostPlatform.system}.sentry-cli
    (pkgs.callPackage ../../pkgs/claude-swap.nix { })
  ];

  programs.orca = {
    enable = true;
    integrations = {
      claude.enable = true;
    };
  };

  programs.playwright-cli = {
    enable = true;
    package = derivations.playwright-cli;
    chromium = {
      enable = true;
      # Uses the NixOS setuid helper configured in nixos/llm.nix.
      sandbox = true;
    };
    camoufox.enable = true;
  };

  programs.nono = {
    enable = true;
    package = derivations.nono;
    profiles = {
      claude-haf = {
        extends = "claude-code";
        meta.description = "claude-code pack + ccstatusline, sentry, playwright and cargo access";
        filesystem = {
          read = config.programs.playwright-cli.filesystem.read ++ [
            "$XDG_CONFIG_HOME/ccstatusline"
            "$HOME/.cargo"
          ];
          # gitconfig's includeIf for ~/work; git hard-fails on unreadable includes
          read_file = [
            "$HOME/.config/sops-nix/secrets/gitWorkConfig"
            "$XDG_CONFIG_HOME/orca/agent-hooks/endpoint.env"
          ];
          allow = config.programs.playwright-cli.filesystem.write ++ [
            "$HOME/.od"
            "$HOME/.orca"
            "$HOME/.sentry"
            # /ponytail <level> persists the default mode here
            "$XDG_CONFIG_HOME/ponytail"
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
    };
  };

  programs.claude-code = {
    enable = true;
    enableMcpIntegration = true;
    package = derivations.claude-code;
    settings = {
      model = "claude-fable-5-1";
      outputStyle = "Concise";
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
    # Stape's hosted GTM server; OAuth login happens in Claude Code via /mcp
    servers.google-tag-manager.url = "https://gtm-mcp.stape.ai/mcp";
  };

  programs.agent-skills = {
    claude-code.enable = true;
    opencode = {
      enable = true;
      # opencode uses the singular directory, unlike the module default
      directory = ".config/opencode/skill";
    };
    skills = {
      playwright-cli.enable = true;
      sentry-cli.enable = true;
      ai-assisted-code-review.enable = true;
      claude-plugins.frontend-design.enable = true;
      matt-pocock.enable = true;
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
