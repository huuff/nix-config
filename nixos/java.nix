{ pkgs, ... }:
{
  environment = {
    systemPackages = with pkgs; [ jdk11_headless ];

    etc = with pkgs; {
      "jdk11".source = jdk11_headless;
      "jdk16".source = jdk16_headless;
    };
  };

  system.activationScripts = {
    # we use this at work
    # it doesn't work, "read-only filesystem"
    # do it manually for the time being
    createProdJDK = ''
        ln -s /etc/jdk11 /opt/prod_jdk || true
      '';
  };
}
