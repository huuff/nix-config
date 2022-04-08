{ pkgs, ... }:
{
  environment = {
    systemPackages = with pkgs; [ 
      jdk17_headless
    ];

    etc = with pkgs; {
      "jdk17".source = jdk17_headless;
    };
  };

  systemd.tmpfiles.rules = [
    "L /opt/jdk17 - - - - /etc/jdk17"
  ];
}
