{
  lib,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  dbus,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "nono";
  version = "0.37.1";

  src = fetchFromGitHub {
    owner = "always-further";
    repo = "nono";
    tag = "v${finalAttrs.version}";
    hash = "sha256-0x+/Kga8dJFnCLpy9McHZqL5bYSghpGPQtKmsSWxt7k=";
  };

  cargoHash = "sha256-i1OiZmdZlPZHJom9eYW5BQs42edjvO00tiiJl2wCgd0=";

  nativeBuildInputs = [
    pkg-config
  ];

  buildInputs = [
    dbus
  ];

  doCheck = false;

  meta = {
    description = "Secure, kernel-enforced sandbox for AI agents, MCP and LLM workloads";
    homepage = "https://github.com/always-further/nono";
    changelog = "https://github.com/always-further/nono/blob/${finalAttrs.src.rev}/CHANGELOG.md";
    license = lib.licenses.asl20;
    mainProgram = "nono";
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
  };
})
