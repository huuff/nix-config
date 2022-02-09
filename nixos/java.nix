{ pkgs, ... }:
{
  environment = {
    systemPackages = with pkgs; [ jdk11_headless jdk17_headless ];

    etc = with pkgs; {
      "jdk11".source = jdk11_headless;
      "jdk17".source = jdk17_headless;
    };
  };

  system.activationScripts = {
    # we use this at work
    # it doesn't work, "read-only filesystem"
    # do it manually for the time being
    # TODO: Try with tmpfiles.d
    createProdJDK = ''
        ln -s /etc/jdk11 /opt/prod_jdk || true
        ln -s /etc/jdk17 /opt/jdk17 || true
      '';
  };
}
