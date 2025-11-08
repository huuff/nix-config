{ ... }:
{
  programs.starship = {
    enable = true;
    settings = {
      format = "$all$kubernetes$cmd_duration$line_break$character";
      add_newline = false;
      kubernetes = {
        disabled = false;
        format = "on [⎈ $cluster(\\($namespace\\))](blue) ";
      };
    };

  };
}
