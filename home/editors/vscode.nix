{ pkgs, ... }:

{
  # TODO: css modules
  # TODO: Pretty ts errors?
  # TODO: Maybe remove blame for gitlens?
  programs.vscode = {
    enable = true;
    mutableExtensionsDir = false;
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
      wix.vscode-import-cost # show cost of importing js packages
      dbaeumer.vscode-eslint # eslint integration
      pkief.material-icon-theme # nice icon theme
      # TODO: I don't think this is working...
      # is it automatically picking up direnv?
      mkhl.direnv # direnv integration
      waderyan.gitblame # git blame in the bottom bar
      formulahendry.auto-rename-tag # auto rename matching tags in html
      christian-kohler.path-intellisense # autocomplete paths
      firefox-devtools.vscode-firefox-debug # allow debugging with firefox
      usernamehw.errorlens # nicer error inlays
    ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
       
      {
        # some comment niceties?
        name = "better-comments";
        publisher = "aaron-bond";
        version = "3.0.2";
        sha256 = "hQmA8PWjf2Nd60v5EAuqqD8LIEu7slrNs8luc3ePgZc=";
      } 
    ];
  };
}
