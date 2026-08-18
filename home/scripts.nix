{ lib, pkgs, ... }:
let
  writeNuApplication =
    {
      name,
      runtimeInputs ? [ ],
      text,
    }:
    pkgs.writers.writeNuBin name {
      makeWrapperArgs = [
        "--prefix"
        "PATH"
        ":"
        (lib.makeBinPath runtimeInputs)
      ];
    } text;

  scripts = {
    # Creates a backup of a file/directory and makes it read-only to avoid
    # accidentally deleting it (rm asks for confirmation on read-only files).
    bakup = writeNuApplication {
      name = "bakup";
      runtimeInputs = [ pkgs.coreutils ];
      text = ''
        def main [target: path] {
          if not ($target | path exists) {
            error make { msg: $"File ($target) does not exist" }
          }

          mut next = 1
          loop {
            let backup = $"($target).bak.($next)"
            if not ($backup | path exists) {
              cp -r $target $backup
              chmod -w $backup
              break
            }
            $next += 1
          }
        }
      '';
    };

    # Prevents the display from going idle by periodically generating harmless
    # virtual keyboard activity.
    nosleep = writeNuApplication {
      name = "nosleep";
      runtimeInputs = [ pkgs.wtype ];
      text = ''
        def main [] {
          print "Keeping the display awake; press Ctrl+C to stop."
          loop {
            wtype -M shift -m shift
            sleep 30sec
          }
        }
      '';
    };

    # Cleans old Nix generations and unused Docker resources.
    nuke-all = writeNuApplication {
      name = "nuke-all";
      runtimeInputs = with pkgs; [
        docker-client
        home-manager
        nix
        sudo
      ];
      text = ''
        def main [] {
          print "Expiring old Home Manager generations..."
          home-manager expire-generations "-1 days"
          print "Collecting unused Nix store paths..."
          sudo nix-collect-garbage -d

          print "Removing all Docker containers, images, and volumes..."
          let containers = (docker ps -aq | lines)
          if not ($containers | is-empty) {
            docker rm -vf ...$containers
          }

          let images = (docker images -aq | lines)
          if not ($images | is-empty) {
            docker rmi -f ...$images
          }

          let volumes = (docker volume ls -q | lines)
          if not ($volumes | is-empty) {
            docker volume rm ...$volumes
          }

          docker system prune -af --volumes
        }
      '';
    };

    # Deletes merged branches other than main or master.
    git-prune-branches = writeNuApplication {
      name = "git-prune-branches";
      runtimeInputs = [ pkgs.git ];
      text = ''
        def main [] {
          let branches = (
            git branch
            | lines
            | each { |branch| $branch | str trim | str replace --regex '^\* ' "" }
            | where { |branch| $branch != "main" and $branch != "master" }
          )

          if not ($branches | is-empty) {
            git branch -d ...$branches
          }
        }
      '';
    };

    # Removes a key from SSH known hosts.
    ssh-removekey = writeNuApplication {
      name = "ssh-removekey";
      runtimeInputs = [ pkgs.gnused ];
      text = ''
        def main [key: string] {
          sed -i $"/($key)/d" $"($env.HOME)/.ssh/known_hosts"
        }
      '';
    };
  };
in
{
  home.packages = lib.attrValues scripts;
}
