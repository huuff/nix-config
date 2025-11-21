{ ... }: {
  services = {
    tailscale = {
      enable = true;
      useRoutingFeatures = "client";
      extraSetFlags = [
        # necessary for subnet routing
        "--accept-routes"
      ];
    };

    mullvad-vpn.enable = true;

    # required for mullvad
    resolved.enable = true;
  };
}
