{ pkgs, lib, ... }:
{
  programs.walker = {
    enable = true;
    runAsService = true;
    config = {
      app_launch_prefix = "${lib.getExe pkgs.app2unit} --";
    };
  };

  # TODO: the provided home-manager module for configuring elephant is outdated
  # and its configuration won't work, this is the hard way of doing it, until it gets updated
  xdg.configFile."elephant/websearch.toml".text = ''
    [[entries]]
    name = "DuckDuckGo"
    prefix = "@dd "
    url = "https://duckduckgo.com?q=%TERM%"
    default = true
  '';
}
