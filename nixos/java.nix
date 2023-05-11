{ pkgs, ... }:
let
  package = pkgs.jdk17;
in
{
  # TODO: What about just programs.java.enable?
  environment = {
    systemPackages = [ package ];

    etc = with pkgs; {
      "jdk17".source = package;
    };
  };

  systemd.tmpfiles.rules = [
    "L /opt/jdk17 - - - - /etc/jdk17"
  ];
}
