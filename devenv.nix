{ ... }:

{
  languages.nix.enable = true;

  treefmt = {
    enable = true;
    config.programs = {
      nixfmt.enable = true;
    };
  };

  git-hooks.hooks = {
    treefmt.enable = true;
  };
}
