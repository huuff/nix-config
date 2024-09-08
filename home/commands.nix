{ pkgs, ... }: {

  lock = "${pkgs.i3lock}/bin/i3lock --nofork --color 000000";

  # set the language to spanish if it isnt or to english
  # otherwise. I need it in case I want to write a ñ or accent
  toggle-lang = pkgs.writeShellScriptBin "toggle.sh" ''
      current_layout=$(setxkbmap -query \
        | grep layout \
        | tr -s ' ' \
        | cut -d' ' -f2)

      if [ "$current_layout" != "es" ]
      then
        setxkbmap es
      else
        setxkbmap us
      fi 
  '';
}
