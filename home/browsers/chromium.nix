{ ... }:
{
  programs.chromium = {
    enable = true;
    extensions = [
      { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock origin
      { id = "eadndfjplgieldjbigjakmdgkmoaaaoc"; } # xdebug helper
      { id = "jnihajbhpnppcggbcgedagnkighmdlei"; } # livereload
      { id = "aeblfdkhhhdcdjpifhhbdiojplfjncoa"; } # 1password
      { id = "jdkknkkbebbapilgoeccciglkfbmbnfm"; } # apollo client devtools
      { id = "ahfhijdlegdabablpippeagghigmibma"; } # web vitals
      { id = "blaaajhemilngeeffpbfkdjjoefldkok"; } # leechblock
    ];
  };

}
