{pkgs, ...}:

{
  programs.neovim = {
    enable = true;

    vimAlias = true;
    viAlias = true;

    withNodeJs = true;

    # TODO: Should add coc-phpls but it's not in nixpkgs
    extraConfig = builtins.readFile ./config.vim;
    plugins = with pkgs.vimPlugins; [
      # Languages
      vim-nix
      vim-twig
      vim-vagrant
      kotlin-vim
      #coc-vetur # TODO: Currently using coc-volar, but it's not in nixpkgs so just doing :CocInstall @yaegassy/coc-volar
      coc-tsserver
      coc-eslint
      coc-yaml
      coc-html
      coc-rls

      # Convenience
      vim-surround
      vim-visual-multi # start a cursor on a selection with `^N`, use `m<obj>` to match all occurrences inside text object
      vim-textobj-entire # provides `ae` as a textobject for the whole buffer
      vim-argumentative # shift arguments with `<,` and `>,`, move between argument boundaries with `[,` and `],`
      fzf-vim # TODO: Not sure how to use it
      fugitive # I usually only do :Git blame honestly
      coc-nvim
      coc-pairs # TODO: not sure I like it
      #coc-snippets # commented because I'm not using it, loads on every file and it's slow
      coc-diagnostic
      nerdcommenter # <spc>C<spc> to toggle comment
      vim-gitgutter # git indicators in the gutter
      vim-repeat # allows me to repeat some plugin commands with .
      suda-vim # use :SudaWrite to save as sudo
      undotree # run :UndotreeTogglet to see the panel
      vim-sneak # use s to act like f but with two characters and in the whole buffer
      vim-airline
      vim-indent-guides # show indentation level, I need it for yaml. TODO: configure it so it looks nice, turn it on by default for yaml
      vim-grammarous # run :GrammarousCheck to check grammar TODO: Set it to run automatically in `.md` files?

      # Markdown
      vim-pandoc-syntax
    ];

    extraPackages = with pkgs; [
      watchman # coc-volar needs it
    ];
  };



  # TODO: Looks like the home-manager module now has an option for coc huh
  xdg.configFile."nvim/coc-settings.json".text = builtins.readFile ./coc-settings.json;
}
