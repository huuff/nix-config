def match(command):
    return ('command not found' in command.output.lower())

def get_new_command(command):
    return f'nix run nixpkgs#{command.script}'

enabled_by_default = True;
