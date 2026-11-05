{ buildNpmPackage, src }:
buildNpmPackage {
  pname = "playwright-cli";
  version = "0.1.23";
  inherit src;
  npmDepsHash = "sha256-GQjH0Zwf9cYuuM6m4mM2Xwz6qWuJTsXsj0nCSm71BlY=";
  dontNpmBuild = true;
}
