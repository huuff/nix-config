{ python3Packages, fetchPypi }:
python3Packages.buildPythonApplication rec {
  pname = "claude-swap";
  version = "0.26.0";
  pyproject = true;
  src = fetchPypi {
    pname = "claude_swap";
    inherit version;
    hash = "sha256-9H8BPvYnXzYKYAAPWGT1VpfWK7a0gxAMTfcpbkaoSdc=";
  };
  build-system = [ python3Packages.hatchling ];
  dependencies = with python3Packages; [
    textual
    truststore
  ];
  # keyring is win32-only
  pythonRemoveDeps = [ "keyring" ];
  doCheck = false;
}
