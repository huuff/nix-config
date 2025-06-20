{pkgs, ...}:

{
  programs.neovim = {
    enable = true;

    vimAlias = true;
    viAlias = true;

    withNodeJs = true;

    # TODO: The appended config is for coc... shall I merge with config.vim?
    # (or maybe even start using lua?)
    extraConfig = builtins.readFile ./config.vim + ''
      " Rename symbol with <leader>rn
      nmap <leader>rn <Plug>(coc-rename)

      " Use K to show documentation in preview window
      nnoremap <silent> K :call <SID>show_documentation()<CR>

      function! s:show_documentation()
        if (index(['vim','help'], &filetype) >= 0)
          execute 'h '.expand('<cword>')
        else
          call CocAction('doHover')
        endif
      endfunction
    '';

    # TODO: Should add coc-phpls but it's not in nixpkgs
    plugins = with pkgs.vimPlugins; [
      # Languages
      vim-nix
      vim-twig
      vim-helm
      kotlin-vim
      coc-tsserver
      coc-eslint
      coc-yaml
      coc-html
      coc-go
      ron-vim # Rust Object Notation

      # Convenience
      vim-surround
      vim-visual-multi # start a cursor on a selection with `^N`, use `m<obj>` to match all occurrences inside text object
      vim-textobj-entire # provides `ae` as a textobject for the whole buffer
      vim-argumentative # shift arguments with `<,` and `>,`, move between argument boundaries with `[,` and `],`
      fzf-vim # TODO: Not sure how to use it
      fugitive # I usually only do :Git blame honestly
      coc-pairs # TODO: not sure I like it
      #coc-snippets # commented because I'm not using it, loads on every file and it's slow
      coc-diagnostic
      nerdcommenter # <spc>C<spc> to toggle comment
      vim-gitgutter # git indicators in the gutter
      vim-repeat # allows me to repeat some plugin commands with .
      vim-suda # use :SudaWrite to save as sudo
      undotree # run :UndotreeTogglet to see the panel
      vim-sneak # use s to act like f but with two characters and in the whole buffer
      vim-airline
      vim-indent-guides # show indentation level, I need it for yaml. TODO: configure it so it looks nice, turn it on by default for yaml
      vim-grammarous # run :GrammarousCheck to check grammar TODO: Set it to run automatically in `.md` files?

      # Markdown
      vim-pandoc-syntax
    ];

    coc = {
      enable = true;
      settings = {
      # TODO: Maybe inline the pkgs of the languageservers here? in the commands? e.g. command = "${pkgs.rnix-lsp}/bin/rnix-lsp"
      languageserver = {
        nix = {
          command = "nil";
          filetypes = [ "nix" ];
        };

        bash = {
          command = "bash-language-server";
          args = [ "start" ];
          filetypes = [ "sh" ];
          ignoredRootPaths = [ "~" ];
        };
      };

      eslint = {
        probe = [ "javascript" "javascriptreact" "typescript" "typescriptreact" "html" "vue"];
      };

      diagnostic-languageserver = {
        filetypes = {
          sh = "shellcheck";
        };

        formatFileTypes = {
          sh = "shfmt";
        };
      };

      yaml = {
        schemas = {
          "https://raw.githubusercontent.com/OAI/OpenAPI-Specification/main/schemas/v3.0/schema.json" = [ "*api.yaml" ];
        };
      };

      go = {
        goplsOptions = {
          templateExtensions = [ "gotmpl" ];
        };
      };
    };
  };
};
}
