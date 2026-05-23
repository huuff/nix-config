{ writeShellApplication, jq }:
# Inlined nono sandbox diagnostics. Upstream's script ships with a /bin/bash
# shebang that doesn't exist on NixOS, so we re-bundle it through
# writeShellApplication for a store-path shebang and a pinned jq.
writeShellApplication {
  name = "nono-hook";
  runtimeInputs = [ jq ];
  text = ''
    if [ -z "''${NONO_CAP_FILE:-}" ] || [ ! -f "$NONO_CAP_FILE" ]; then
      exit 0
    fi

    CAPS=$(jq -r '.fs[] | "  " + (.resolved // .path) + " (" + .access + ")"' "$NONO_CAP_FILE" 2>/dev/null || true)
    NET=$(jq -r 'if .net_blocked then "blocked" else "allowed" end' "$NONO_CAP_FILE" 2>/dev/null || true)

    CONTEXT="[NONO SANDBOX - PERMISSION DENIED]

    STOP. Do not try alternative approaches. This is a hard security boundary.

    You are running inside the nono security sandbox. The operation you just attempted is PERMANENTLY BLOCKED for this session. There is NO workaround possible from inside this session.

    ALLOWED PATHS (everything else is blocked):
    $CAPS
    Network: $NET

    FORBIDDEN ACTIONS - DO NOT ATTEMPT:
    - Trying alternative file paths
    - Copying files to allowed locations
    - Using sudo or changing permissions
    - Suggesting the user run commands manually
    - Any other workaround

    REQUIRED ACTION:
    Tell the user they must EXIT this Claude session and restart with the needed path:

      nono run --allow /path/to/needed -- claude

    This is the ONLY solution. The sandbox cannot be modified from within."

    jq -n --arg ctx "$CONTEXT" '{
      "hookSpecificOutput": {
        "hookEventName": "PostToolUseFailure",
        "additionalContext": $ctx
      }
    }'
  '';
}
