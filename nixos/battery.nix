{ ... }: {

  services.tlp = {
    enable = true;

    settings = {
      # Don't charge above 80 and don't start charging before 40
      # This is healthier for the battery
      START_CHARGE_THRESH_BAT0 = 40;
      STOP_CHARGE_THRESH_BAT0 = 80;

      # Performance mode on AC, powersave otherwise
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      # Enable boost on AC
      CPU_BOOST_ON_AC = 1;
      CPU_BOOST_ON_BAT = 0;
    };
  };

}
