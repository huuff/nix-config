{ ... }:
{
  programs.taskwarrior.enable = true;

  programs.zsh.initContent = ''
    # Make subcommands like `task done` complete task IDs (shown via fzf-tab)
    for cmd in done modify delete start stop edit annotate denotate duplicate information; do
      eval "_task_''${cmd}() { _task_ids }"
    done
  '';
}
