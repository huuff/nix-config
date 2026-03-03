{ superpowers, ... }:
let
  skillNames = builtins.attrNames (builtins.readDir "${superpowers}/skills");
in
{
  # programs.opencode.skills only recognizes Nix path literals as directories
  # (lib.isPath check), so string-interpolated store paths from flake inputs
  # get written as text content instead of symlinked. We use xdg.configFile
  # directly to get proper recursive symlinks.
  xdg.configFile =
    builtins.listToAttrs (
      map (name: {
        name = "opencode/skill/${name}";
        value = {
          source = "${superpowers}/skills/${name}";
          recursive = true;
        };
      }) skillNames
    )
    // {
      "opencode/plugins/superpowers.js".source = "${superpowers}/.opencode/plugins/superpowers.js";
    };
}
