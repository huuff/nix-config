{ pkgs, ... }:

{
  # XXX: Postman is broken in nixpkgs because they only provided links to the latest version
  # so I have to use internet archive to download an older version
  nixpkgs.overlays = [
    (final: prev: {
      postman = prev.postman.overrideAttrs(old: rec {
        version = "20231205182607";
        src = final.fetchurl {
          url = "https://web.archive.org/web/${version}/https://dl.pstmn.io/download/latest/linux_64";
          sha256 = "sha256-PthETmSLehg6eWpdDihH1juwiyZdJvzr+qyG2YYuEZI=";
          name = "${old.pname}-${version}.tar.gz";
        };
      });
    })
  ];

  home.packages = [
    pkgs.postman
  ];
}
