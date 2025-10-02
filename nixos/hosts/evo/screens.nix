{ pkgs, ... }: {

  services.xserver.displayManager.setupCommands = "${pkgs.autorandr}/bin/autorandr --change";
  services.autorandr = {
    enable = true;
    profiles = 
    let
      laptopScreenFingerprint =  "00ffffffffffff000e7731140000000000210104b51e1378032f55a6544c9b240d505400000001010101010101010101010101010101178840a0b0086e70302066002dbc10000018000000fd001e78e6e646010a202020202020000000fe0043534f542054330a2020202020000000fc004d4e453030375a41332d320a2001cc7020790200220014bfa10a853f0b9f002f001f0007076d00050005002b000c27001e77000027001e3b0000810015741a000003511e780000000000007800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008b90";
      hp240aFingerprint =  "00ffffffffffff00220e993401010101111c010380351e782a0565a756529c270f5054a10800d1c081c0a9c0b3009500810081800101023a801871382d40582c45000f282100001e000000fd00323c1e5011000a202020202020000000fc004850205648323430610a202020000000ff0036434d383137333959590a20200122020322f149901f04130312021101230907078301000068030c001000002200e2002b023a801871382d40582c45000f282100001e023a80d072382d40102c45800f282100001e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f9";
      officeLgFingerprint = "00ffffffffffff001e6ddf5b00000000011a010380301b78ea3135a5554ea1260c5054a54b00714f81809500b300a9c0810081c09040023a801871382d40582c4500e00e1100001e000000fd00384b1e5512000a202020202020000000fc004c472046554c4c2048440a2020000000ff000a20202020202020202020202001db020312b14790040301121f1365030c001000023a801871382d40582c4500e00e1100001e2a4480a07038274030203500e00e1100001e011d007251d01e206e285500e00e1100001e8c0ad08a20e02d10103e9600e00e110000180000000000000000000000000000000000000000000000000000000000000000000000000063";
      laptopScreenConfig = {
        enable = true;
        primary = true;
        mode = "2880x1800";
        position = "0x0";
        scale = {
          x = 0.5;
          y = 0.5;
        };
      };
    in
    {
      portable = {
        fingerprint = {
          "eDP-1" = laptopScreenFingerprint;
        };
        config = {
          "eDP-1" = laptopScreenConfig;
        };
      };
      docked = {
        fingerprint = {
          "eDP-1" = laptopScreenFingerprint;
          "DP-1" = hp240aFingerprint;
        };

        config = {
          "eDP-1" = laptopScreenConfig;
          "DP-1" = {
            enable = true;
            mode = "1920x1080";
            position = "2880x0";
          };
        };
      };
      office = {
        fingerprint = {
          "eDP-1" = laptopScreenFingerprint;
          "HDMI-1" = officeLgFingerprint;
        };

        config = {
          "eDP-1" = laptopScreenConfig;
          "HDMI-1" = {
            enable = true;
            mode = "1920x1080";
            position = "2880x0";
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
