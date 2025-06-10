{ pkgs, ... }: {

  services.xserver.displayManager.setupCommands = "${pkgs.autorandr}/bin/autorandr --change";
  services.autorandr = {
    enable = true;
    profiles = 
    let
      laptopScreenFingerprint =  "00ffffffffffff004c835841000000000a1e0104b51d1178020cf1ae523cb9230c505400000001010101010101010101010101010101bc3680b4703820403020880026a51000001bbc3680b4703820403020880026a51000001b0000000f00d1093cd1093c28800000000000000000fe0041544e413333584331312d3020016102030f00e3058000e606050174600700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000b7";
      hp240aFingerprint =  "00ffffffffffff00220e993401010101111c010380351e782a0565a756529c270f5054a10800d1c081c0a9c0b3009500810081800101023a801871382d40582c45000f282100001e000000fd00323c1e5011000a202020202020000000fc004850205648323430610a202020000000ff0036434d383137333959590a20200122020322f149901f04130312021101230907078301000068030c001000002200e2002b023a801871382d40582c45000f282100001e023a80d072382d40102c45800f282100001e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f9";
      officeLgFingerprint = "00ffffffffffff001e6ddf5b00000000011a010380301b78ea3135a5554ea1260c5054a54b00714f81809500b300a9c0810081c09040023a801871382d40582c4500e00e1100001e000000fd00384b1e5512000a202020202020000000fc004c472046554c4c2048440a2020000000ff000a20202020202020202020202001db020312b14790040301121f1365030c001000023a801871382d40582c4500e00e1100001e2a4480a07038274030203500e00e1100001e011d007251d01e206e285500e00e1100001e8c0ad08a20e02d10103e9600e00e110000180000000000000000000000000000000000000000000000000000000000000000000000000063";
    in
    {
      portable = {
        fingerprint = {
          "eDP-1" = laptopScreenFingerprint;
        };
        config = {
          "eDP-1" = {
            enable = true;
            primary = true;
            mode = "1920x1080";
          };
        };
      };
      docked = {
        fingerprint = {
          "eDP-1" = laptopScreenFingerprint;
          "DP-1" = hp240aFingerprint;
        };

        config = {
          "eDP-1" = {
            enable = true;
            primary = true;
            mode = "1920x1080";
            position = "0x0";
          };
          "DP-1" = {
            enable = true;
            mode = "1920x1080";
            position = "1920x0";
          };
        };
      };
      office = {
        fingerprint = {
          "eDP-1" = laptopScreenFingerprint;
          "HDMI-1" = officeLgFingerprint;
        };

        config = {
          "eDP-1" = {
            enable = true;
            primary = true;
            mode = "1920x1080";
            position = "0x0";
          };
          "HDMI-1" = {
            enable = true;
            mode = "1920x1080";
            position = "1920x0";
          };
        };
      };
    };
    hooks = {
      postswitch = {
        # TODO: embed the package to ensure it's always present
        notify = ''notify-send "Switched screen profile" "$AUTORANDR_CURRENT_PROFILE"'';
      }; 
    };
  };


}
