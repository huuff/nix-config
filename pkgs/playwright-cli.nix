{ buildNpmPackage, src }:
buildNpmPackage {
  pname = "playwright-cli";
  version = "0.1.1";
  inherit src;
  npmDepsHash = "sha256-ZrO8yIqMYMQUlsQraejVgKRZ7klC5/8UsV3/H1EqYtA=";
  dontNpmBuild = true;
}
