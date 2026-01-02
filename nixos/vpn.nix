{ ... }:
{
  services = {
    tailscale = {
      enable = true;
      useRoutingFeatures = "client";
      extraSetFlags = [
        # necessary for subnet routing
        "--accept-routes"
      ];
    };

    # TODO: removed mullvad because it broke my internet
    # (I fear it might be tailscale + resolved that breaks it
    # as it has happened before)
    # mullvad-vpn.enable = true;

    # required for mullvad
    # resolved.enable = true;
  };
}
