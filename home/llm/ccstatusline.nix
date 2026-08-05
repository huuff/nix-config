{
  pkgs,
  ponytail,
  ...
}:
{
  programs.ccstatusline = {
    enable = true;
    settings = {
      # default layout (see SettingsSchema in ccstatusline) + LLM controls
      lines = [
        [
          {
            id = "1";
            type = "model";
            color = "cyan";
          }
          {
            id = "2";
            type = "separator";
          }
          {
            id = "3";
            type = "thinking-effort";
            color = "magenta";
          }
          {
            id = "4";
            type = "separator";
          }
          {
            id = "ponytail";
            type = "custom-command";
            commandPath = "${pkgs.bash}/bin/bash ${ponytail}/hooks/ponytail-statusline.sh";
            preserveColors = true;
          }
          {
            id = "ponytail-separator";
            type = "separator";
          }
          {
            id = "5";
            type = "context-length";
            color = "brightBlack";
          }
          {
            id = "6";
            type = "separator";
          }
          {
            id = "7";
            type = "git-branch";
            color = "magenta";
          }
          {
            id = "8";
            type = "separator";
          }
          {
            id = "9";
            type = "git-changes";
            color = "yellow";
          }
        ]
      ];
    };
  };
}
