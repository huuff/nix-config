{ ... }: {
  services = {
    tailscale = {
      enable = true;
      useRoutingFeatures = "client";
    };

    mullvad-vpn.enable = true;

    # required for mullvad
    resolved.enable = true;
  };
}
