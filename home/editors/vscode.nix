{ pkgs, ... }:

{
  # TODO: css modules
  # TODO: Maybe remove blame for gitlens?
  programs.vscode = {
    enable = true;
    # prevents manually installing extensions, but also prevents nix-installed versions
    # from randomly breaking
    mutableExtensionsDir = false;
    profiles.default = {
      keybindings = [ # Some keybindings as in Intellij IDEA
      # TODO: Maybe use the idea keybindings plugin instead of doing my own
        {
          key = "ctrl+alt+l";
          command = "editor.action.formatDocument";
        }

        {
          key = "ctrl+alt+s";
          command = "workbench.action.files.saveAll";
        }

        {
          key = "shift+f6";
          command = "editor.action.rename";
          when = "editorHasRenameProvider && editorTextFocus && !editorReadonly";
        }

        {
          key = "shift shift";
          command = "workbench.action.quickOpen";
        }
      ];
      extensions = with pkgs.vscode-extensions; [
        pkief.material-icon-theme # nice icon theme
        mkhl.direnv # direnv integration
        # TODO: is this necessary? I think I've read this is native in vscode
        formulahendry.auto-rename-tag # auto rename matching tags in html
        christian-kohler.path-intellisense # autocomplete paths
        firefox-devtools.vscode-firefox-debug # allow debugging with firefox
        usernamehw.errorlens # nicer error inlays
        rust-lang.rust-analyzer # rust-analyzer
        tamasfe.even-better-toml # TOML support
        aaron-bond.better-comments # nicer display for todos and such
        jnoortheen.nix-ide # nix support
        bradlc.vscode-tailwindcss # tailwind class autocomplete
        vadimcn.vscode-lldb # rust debugger
        james-yu.latex-workshop # latex all-in-one
        github.vscode-pull-request-github # do pr reviews in vscode
        vscodevim.vim # vim emulation layer
        hashicorp.terraform 

        # themes
        jdinhlife.gruvbox 
        enkia.tokyo-night
        arcticicestudio.nord-visual-studio-code
      ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
        {
          name = "theme-by-language";
          publisher = "jsaulou";
          version = "1.3.0";
          sha256 = "VyXIGPraAABt5bOJH86qvWtbCY2Tp99z26w2qUOCcsQ=";
        }
      ];

      userSettings = {
        "workbench.iconTheme" = "material-icon-theme";

        "tailwindCSS.experimental.classRegex" = [
          # for dioxus
          "class: \"(.*)\""
        ];
        "tailwindCSS.includeLanguages" = {
            # rust frontend frameworks
            "rust" = "html";
            "html" = "html";
          };

        # suggested setting for tailwind
        "editor.quickSuggestions" = {
          "strings" = true;
        };

        "git.autofetch" = true;

        "nix" = {
          "enableLanguageServer" = true;
          "serverPath" = "nil"; # don't provide path from `pkgs` so the binary is taken from devshell
          "serverSettings"= {
            "nil" = {
              "formatting" = {
                "command" = ["nixfmt"]; # same as above
              };
            };
          };
        };

        "theme-by-language" = {
          "themes" = {
            "*" = "Default Dark Modern";
            "rust" = "Gruvbox Dark Hard";
            "nix" = "Tokyo Night";
          };
        };

        "vim" = {
          "useSystemClipboard" = true;
          "leader" = "<space>";
          "sneak" = true;
          "useCtrlKeys" = true;
          "handleKeys" = {
            # let me use ^w to switch windows instead of closing
            # them
            "<C-w>" = true;
            # keep vscode's native ^d for multicursor
            "<C-d>" = false;
          };

          "normalModeKeyBindings" = [
            { "before" = [ "[" "l" ]; "commands" = [ "editor.action.marker.prevInFiles" ]; "when" = "editorTextFocus"; }
            { "before" = [ "]" "l" ]; "commands" = [ "editor.action.marker.nextInFiles" ]; "when" = "editorTextFocus"; }
          ];
        };

        "better-comments.tags" = [
          {
            "tag" = "todo";
            "color" = "#FF8C00";
            "strikethrough" = false;
            "underline" = false;
            "backgroundColor" = "transparent";
            "bold" = false;
            "italic" = false;
          }
          {
            "tag" = "xxx";
            "color" = "#FF2D00";
            "strikethrough" = false;
            "underline" = false;
            "backgroundColor" = "transparent";
            "bold" = false;
            "italic" = false;
          }
          {
            "tag" = "maybe";
            "color" = "#3498DB";
            "strikethrough" = false;
            "underline" = false;
            "backgroundColor" = "transparent";
            "bold" = false;
            "italic" = false;
          }
        ];
      };
    };
  };
}
