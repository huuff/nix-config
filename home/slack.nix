{ pkgs, ... }:
{

  # XXX Weird stuff that may be needed so slack works decently in wayland
  # (i.e., without it it feels unpleasantly slow, and screen sharing won't work)
  home.packages = [
    (pkgs.symlinkJoin {
      name = "slack-wayland";
      paths = [ pkgs.slack ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/slack \
          --add-flags "--disable-gpu-compositing" \
          --add-flags "--enable-features=UseOzonePlatform" \
          --add-flags "--ozone-platform=wayland" \
          --add-flags "--enable-wayland-ime" \
          --add-flags "--enable-features=WebRTCPipeWireCapturer" \
          --add-flags "--enable-features=WaylandWindowDecorations" \
          --add-flags "--disable-features=WaylandFractionalScaleV1"
      '';
    }) 
  ];
}
