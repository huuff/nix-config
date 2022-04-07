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

  #system.activationScripts = {
    ## we use this at work
    ## it doesn't work, "read-only filesystem"
    ## do it manually for the time being
    ## TODO: Try with tmpfiles.d
    #createProdJDK = ''
        ## ln -s /etc/jdk11 /opt/prod_jdk || true
        #ln -s /etc/jdk17 /opt/jdk17 || true
      #'';
  #};
}
