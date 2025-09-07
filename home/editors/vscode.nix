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
        formulahendry.auto-rename-tag # auto rename matching tags in html
        christian-kohler.path-intellisense # autocomplete paths
        firefox-devtools.vscode-firefox-debug # allow debugging with firefox
        usernamehw.errorlens # nicer error inlays
        rust-lang.rust-analyzer # rust-analyzer
        tamasfe.even-better-toml # TOML support
        aaron-bond.better-comments # nicer display for todos and such
        # TODO: I think I'm missing LSP config and others
        jnoortheen.nix-ide # nix support
      ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
        # XXX: I'm leaving this one as an example in case I need to manually add some, but this one's already in
        # nixpkgs
        #{
          ## some comment niceties?
          #name = "better-comments";
          #publisher = "aaron-bond";
          #version = "3.0.2";
          #sha256 = "hQmA8PWjf2Nd60v5EAuqqD8LIEu7slrNs8luc3ePgZc=";
        #}
      ];
    };
  };
}
