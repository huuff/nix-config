{ pkgs, ... }:

{
  # TODO: css modules
  # TODO: Pretty ts errors?
  # TODO: Maybe remove blame for gitlens?
  programs.vscode = {
    enable = true;
    keybindings = [ # Some keybindings as in Intellij IDEA
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
      bradlc.vscode-tailwindcss
      wix.vscode-import-cost
      dbaeumer.vscode-eslint
      pkief.material-icon-theme
      # TODO: I don't think this is working...
      # is it automatically picking up direnv?
      mkhl.direnv
      waderyan.gitblame
      formulahendry.auto-rename-tag
      christian-kohler.path-intellisense
      firefox-devtools.vscode-firefox-debug
    ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "vscode-jest";
        publisher = "Orta";
        version = "5.0.3";
        sha256 = "9v2de1ETxe3A18Wq1fnYl+f65UsNPmdpqmBJ8M4p3Wc=";
      } 
      {
        name = "better-comments";
        publisher = "aaron-bond";
        version = "3.0.2";
        sha256 = "hQmA8PWjf2Nd60v5EAuqqD8LIEu7slrNs8luc3ePgZc=";
      } 
    ];
  };
}
