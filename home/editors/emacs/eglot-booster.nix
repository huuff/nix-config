{ pkgs, epkgs }:
epkgs.trivialBuild rec {
  pname = "eglot-booster";
  version = "e19dd7ea81bada84c66e8bdd121408d9c0761fe6";
  src = pkgs.fetchFromGitHub {
    owner = "jdtsmith";
    repo = "eglot-booster";
    rev = "e19dd7ea81bada84c66e8bdd121408d9c0761fe6";
    hash = "sha256-vF34ZoUUj8RENyH9OeKGSPk34G6KXZhEZozQKEcRNhs=";
  };
}
