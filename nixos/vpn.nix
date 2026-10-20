{
  config,
  pkgs,
  lib,
  ...
}:
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

  # lockdown stays on by default; this only relaxes it on trusted ssids (fail closed)
  systemd.services.mullvad-trusted-networks = {
    description = "Toggle mullvad lockdown mode based on the connected wifi network";
    wantedBy = [ "multi-user.target" ];
    after = [
      "iwd.service"
      "mullvad-daemon.service"
    ];
    serviceConfig = {
      Restart = "always";
      RestartSec = 5;
    };
    script = lib.getExe (
      pkgs.writers.writeNuBin "mullvad-trusted-networks"
        {
          makeWrapperArgs = [
            "--prefix"
            "PATH"
            ":"
            (lib.makeBinPath (
              with pkgs;
              [
                mullvad
                dbus
                systemd
              ]
            ))
          ];
        }
        ''
          let trusted = "${config.sops.secrets.trustedNetworks.path}"

          def connected-ssid [] {
            let objects = (
              busctl --json=short call net.connman.iwd / org.freedesktop.DBus.ObjectManager GetManagedObjects
              | from json
              | get data.0
              | transpose path ifaces
            )
            let station = (
              $objects
              | where ($it.ifaces."net.connman.iwd.Station"?.State?.data? == "connected")
              | get -o 0.ifaces."net.connman.iwd.Station"
            )
            if $station == null { return null }
            $objects
            | where path == $station.ConnectedNetwork.data
            | get 0.ifaces."net.connman.iwd.Network".Name.data
          }

          def apply [] {
            let ssid = (connected-ssid)
            let want = if $ssid != null and $ssid in (open $trusted | lines) { "off" } else { "on" }
            let current = (mullvad lockdown-mode get | parse "{_}: {state}" | get 0.state | str trim)
            if $current != $want {
              print $"ssid=($ssid | default '<none>') lockdown=($want)"
              mullvad lockdown-mode set $want
            }
          }

          apply
          dbus-monitor --system "type='signal',sender='net.connman.iwd',interface='org.freedesktop.DBus.Properties',member='PropertiesChanged',path_namespace='/net/connman/iwd'"
          | lines
          | where $it =~ 'string "(State|ConnectedNetwork)"'
          | each { apply }
          | ignore
        ''
    );
  };
}
