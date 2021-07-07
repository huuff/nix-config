secrets: user:
{ config, pkgs, lib, ... }:
let
  mydrvs = builtins.fetchGit {
    url="https://github.com/huuff/derivations.git";
    rev="155235db8fe7d680fb4c3c1a45d70579d29f07e0";
  };
in
{
    imports = [
      ./editors/vim/nvim.nix
      ./editors/emacs/emacs.nix

      ./browsers/firefox.nix
      # ./browsers/surf.nix

      ./terminal-emulators/st.nix
      ./terminal-emulators/alacritty.nix

      "${mydrvs}/scripts.nix"
      "${mydrvs}/autocutsel.nix"

      ./git.nix
      ./bash.nix
      ./starship.nix
      ./i3.nix
    ];


  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.fzf.enable = true;

  haf.scripts.enable = true;
  haf.autocutsel.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = user;
  home.homeDirectory = "/home/${user}";
  home.sessionVariables = {
    EDITOR = "nvim";
    LESS = "--mouse --wheel-lines=3"; # be able to scroll less with mouse
  };

  home.packages = with pkgs; [
    anki
    zathura
    scrot
    jetbrains.idea-ultimate
    xclip
    cloc
    nixpkgs-fmt
    nix-prefetch-git
    pavucontrol
    python3
    ntfs3g
    gnupg
    texlive.combined.scheme-medium
    keepassx2
    himalaya
    _1password
    nixopsUnstable
    sshfs
    rnix-lsp
    vagrant
    libreoffice
    deluge
    slack
    jetbrains.phpstorm
    mycli
    tealdeer # tldr command, for quick manpages
    sshpass
    nodePackages.bash-language-server
    thefuck
    shellcheck
    shfmt
    nodePackages.diagnostic-languageserver
    maven

    simplescreenrecorder
    teams
  ] ++ import ./cli-essentials.nix { inherit pkgs; } ;


  programs.mpv = {
    enable = true;
    config = {
      save-position-on-quit = true;
    };
  };

  programs.tmux = {
    enable = true;
    extraConfig = ''
        set-option -g mouse on
        bind | split-window -h -c "#{pane_current_path}"
        bind - split-window -v -c "#{pane_current_path}"
        bind c new-window -c "#{pane_current_path}"  
    '';
    plugins = with pkgs; [
      tmuxPlugins.yank
      {
        plugin = tmuxPlugins.better-mouse-mode;
        extraConfig = ''
          set-option -g mouse on
          set-option -g @emulate-scroll-for-no-mouse-alternate-buffer on
          '';
      }
    ];
  };

  services.gpg-agent.enable = true;

  programs.lsd = {
    enable = true;
    enableAliases = true;
  };

  programs.ssh = {
    enable = true;
    matchBlocks = secrets.ssh-match-blocks;
  };

  programs.chromium = {
    enable = true;
    extensions = [
      { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock origin
      { id = "eadndfjplgieldjbigjakmdgkmoaaaoc"; } # xdebug helper
    ];
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
