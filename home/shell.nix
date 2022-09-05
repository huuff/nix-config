{ modules, ... }:
{
  imports = [
    modules.shell
  ];


  programs.shell = {
    aliases = {
      perms = ''stat -c "%a %n"'';
      trace-net = ''strace -f -e trace=network''; # is it useful?
      oplogin = "eval $(op signin --account my.1password.com)";
      agip = ''ag "\b\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\b"'';
      ssh = "TERM=xterm-256color ssh"; # for some servers that don't accept my terminals
      syss = "systemctl status";
      sysf = "systemctl --failed";
      sysj = "systemctl list-jobs";
      sysr = "systemctl restart";
      sysc = "systemctl cat";
      jrn = "journalctl -u";
      jrnf = "journalctl -fu";
      sudo= "sudo "; # https://askubuntu.com/questions/22037/aliases-not-available-when-using-sudo/22043#22043 
      watch = "watch "; # same as sudo
      vssh = "TERM=xterm-256color vagrant ssh";
      vup = "vagrant up";
      vhalt = "vagrant halt";
      vss = "vagrant global-status";
      cloc = "cloc --vcs=git";
      ag2 = "ag --context=2";
      duh = "du -h --max-depth=1";
      k = "kubectl";
      hf = "helmfile";
      hm = "himalaya";
      nix-shell = "nix shell"; # Prevent me from ever using nix-shell, since it's older and I'm too used to it
    };

    scriptDir = "/$HOME/scripts";

  };
}
