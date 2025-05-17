{ ... }:
{
  programs.chromium = {
    enable = true;
    extensions = [
      { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock origin
      { id = "aeblfdkhhhdcdjpifhhbdiojplfjncoa"; } # 1password
      { id = "jdkknkkbebbapilgoeccciglkfbmbnfm"; } # apollo client devtools
      { id = "ahfhijdlegdabablpippeagghigmibma"; } # web vitals
      { id = "blaaajhemilngeeffpbfkdjjoefldkok"; } # leechblock
    ];
  };

}
