{ config, lib, ... }:
with lib;
{
  options.users.mainUser = mkOption {
    type = types.str;
    default = null;
    description = "Main user, added to audio, wheel, home-manager";
  };
  
  config = {
    assertions = [
      {
        assertion = config.users.mainUser != null;
        message = "users.mainUser must be set!";
      }
      {
        assertion = config.users.mainUser != "";
        message = "users.mainUser must be non-empty!";
      }
    ];
  };
}
