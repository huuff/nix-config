{ ... }:
{
  programs.ssh = {
    startAgent = true;
    agentTimeout = "1h";
  };
}
