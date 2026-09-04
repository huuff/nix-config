{ ... }:
{
  # LLM agents run playwright-cli inside nono, where unprivileged user
  # namespaces are unavailable. Chromium therefore needs NixOS's sandbox helper
  # at /run/wrappers/bin/__chromium-suid-sandbox. The helper is a small,
  # root-owned setuid program that creates Chromium's process sandbox and then
  # drops privileges; Chromium itself does not run as root. This adds a limited
  # privileged attack surface, but preserves browser isolation and is safer than
  # setting chromiumSandbox = false or passing --no-sandbox.
  security.chromiumSuidSandbox.enable = true;
}
