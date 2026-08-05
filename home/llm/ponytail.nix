{
  pkgs,
  ponytail,
  ...
}:
let
  # hooks invoke bare `node`, which we keep off global PATH
  patched = pkgs.runCommand "ponytail-plugin" { } ''
    cp -r ${ponytail} $out
    chmod -R u+w $out
    substituteInPlace $out/hooks/claude-codex-hooks.json \
      --replace-fail 'node \"''${CLAUDE_PLUGIN_ROOT}' '${pkgs.nodejs}/bin/node \"''${CLAUDE_PLUGIN_ROOT}'
  '';
in
{
  programs.claude-code.plugins = [ patched ];

  # /ponytail <level> persists the default mode here
  programs.nono.profiles.claude-haf.filesystem.allow = [ "$XDG_CONFIG_HOME/ponytail" ];
}
