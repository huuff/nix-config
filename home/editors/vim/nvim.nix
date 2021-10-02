{pkgs, ...}:

{
  programs.neovim = {
    enable = true;

    vimAlias = true;
    viAlias = true;

    withNodeJs = true;

    extraConfig = builtins.readFile ./config.vim;
    plugins = with pkgs.vimPlugins; [
      # Languages
      vim-nix
      vim-twig
      vim-vagrant
      coc-tsserver
      coc-eslint
      coc-yaml

      # Convenience
      vim-surround
      targets-vim
      fzf-vim
      fugitive
      coc-nvim
      #coc-snippets # commented because I'm not using it, loads on every file and it's slow
      coc-diagnostic
      nerdcommenter
      vim-gitgutter
      vim-repeat # allows me to repeat some plugin commands with .
      suda-vim # use :SudaWrite to save as sudo
      undotree
      vim-sneak # use s to act like f but with two characters and in the whole buffer
      vim-airline

      # Markdown
      #mkdx
      #vim-pandoc
      vim-pandoc-syntax
    ];
  };


  xdg.configFile."nvim/coc-settings.json".text = builtins.readFile ./coc-settings.json;
}
