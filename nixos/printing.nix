{
  services.printing.enable = true;

  services.avahi = {
    enable = true;
    openFirewall = true;
    # resolve .local hostnames via mDNS
    nssmdns4 = true;
  };
}
