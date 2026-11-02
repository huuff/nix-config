{ ... }:
{
  services.hyprsunset = {
    enable = true;

    settings = {
      max-gamma = 150;
      # hyprsunset only takes fixed times, no sun-based schedule
      profile = [
        {
          time = "7:30";
          identity = true;
        }
        {
          time = "21:00";
          temperature = 3500;
        }
      ];
    };
  };
}
