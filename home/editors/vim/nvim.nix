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
      coc-vetur
      coc-tsserver
      coc-eslint
      coc-yaml

      # Convenience
      vim-surround
      vim-visual-multi # start a cursor on a selection with `^N`, use `m<obj>` to match all occurrences inside text object
      vim-textobj-entire # provides `ae` as a textobject for the whole buffer
      vim-argumentative # shift arguments with `<,` and `>,`, move between argument boundaries with `[,` and `],`
      fzf-vim # TODO: Not sure how to use it
      fugitive
      coc-nvim
      coc-html
      coc-pairs # TODO: not sure I like it
      #coc-snippets # commented because I'm not using it, loads on every file and it's slow
      coc-diagnostic
      nerdcommenter
      vim-gitgutter # git indicators in the gutter
      vim-repeat # allows me to repeat some plugin commands with .
      suda-vim # use :SudaWrite to save as sudo
      undotree # run :UndotreeTogglet to see the panel
      vim-sneak # use s to act like f but with two characters and in the whole buffer
      vim-airline
      vim-indent-guides # show indentation level, I need it for yaml. TODO: configure it so it looks nice

      # Markdown
      #mkdx
      #vim-pandoc
      vim-pandoc-syntax
    ];
  };


  # TODO: Looks like the home-manager module now has an option for coc huh
  xdg.configFile."nvim/coc-settings.json".text = builtins.readFile ./coc-settings.json;
}
