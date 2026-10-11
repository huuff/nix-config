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

    mullvad-vpn.enable = true;

    # required for mullvad
    resolved.enable = true;
  };

  # mullvad's routing table captures tailnet traffic; the 0x6d6f6c65 mark exempts it
  networking.nftables = {
    enable = true;
    tables.mullvad-exclude-tailscale = {
      family = "inet";
      content = ''
        set tailnet {
          type ipv4_addr
          flags interval
          # CGNAT peers + advertised subnet routes (tailscale status --json .Peer[].PrimaryRoutes)
          elements = { 100.64.0.0/10, 10.0.0.0/16, 10.3.0.0/16, 10.100.0.0/16 }
        }
        chain output {
          type route hook output priority -100; policy accept;
          ip daddr @tailnet ct mark set 0xf41 meta mark set 0x6d6f6c65
          ip6 daddr fd7a:115c:a1e0::/48 ct mark set 0xf41 meta mark set 0x6d6f6c65
        }
        chain input {
          type filter hook input priority -100; policy accept;
          ct mark 0xf41 accept
        }
      '';
    };
  };
}
