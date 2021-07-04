{ config, lib, ... }:
with lib;
let
  user = config.users.mainUser;
in
{
  options.users.mainUser = mkOption {
    type = types.str;
    default = null;
    description = "Main user, added to audio, wheel, home-manager";
  };
  
  config = {
    assertions = [
      {
        assertion = user != null;
        message = "users.mainUser must be set!";
      }
      {
        assertion = user != "";
        message = "users.mainUser must be non-empty!";
      }
    ];

    users.users.${user} = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
    };
  };
}
