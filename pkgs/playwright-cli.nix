{ buildNpmPackage, src }:
buildNpmPackage {
  pname = "playwright-cli";
  version = "0.1.19";
  inherit src;
  npmDepsHash = "sha256-aY3i+sc2p8iQAEpfs+j/ifeBVmMpDDmwctEqOIDmCqI=";
  dontNpmBuild = true;
}
