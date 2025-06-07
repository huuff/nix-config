{ ... }: {

  services.tlp = {
    enable = true;

    settings = {
      # don't charge above 80 and don't start charging before 40
      # this makes my setup much better when plugged all the time
      START_CHARGE_THRESH_BAT0 = 40;
      STOP_CHARGE_THRESH_BAT0 = 80;
    };
  };

}
