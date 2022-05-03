{ pkgs, ... }:
{
  home.packages = with pkgs; [
    vagrant
    docker-compose # TODO: Get rid of it when I get rid of docker (making the switch to podman)
    podman-compose
  ];
}
