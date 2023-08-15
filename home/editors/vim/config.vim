set expandtab " insert spaces on tab
set tabstop=2 " number of spaces inserted on tab
set shiftwidth=2 " number of spaces used for indentation
set mouse=a " enable mouse
set conceallevel=0 " conceal hides information for pretty printing
let g:pandoc#syntax#conceal#use = 0 " same, conceal but for pandoc
set nofoldenable " disable folding
set number " show line numbers
set scrolloff=5 " always show five lines above and below cursor
set wrap linebreak nolist " wrap lines

" use space as leader
nnoremap <SPACE> <Nop>
let mapleader=" "

" start with cursor at last known position
augroup vimStartup
au!

autocmd BufReadPost *
\ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
\ |   exe "normal! g`\""
\ | endif

augroup END

" clear highlighting on escape in normal mode
nnoremap <esc> :noh<return><esc>
nnoremap <esc>^[ <esc>^[

" persistent undotree in a separate file
if has("persistent_undo")
   let target_path = expand('~/.undodir')

    " create the directory and any parent directories
    " if the location does not exist.
    if !isdirectory(target_path)
        call mkdir(target_path, "p", 0700)
    endif

    let &undodir=target_path
    set undofile
endif

" move lines using C-up and C-down
:nnoremap <C-Up> <Up>"add"ap<Up>
:nnoremap <C-Down> "add"ap

" search ignores cases unless an uppercase is used
set ignorecase
set smartcase

" use vim-pandoc-syntax (this is necessary if not using vim-pandoc)
augroup pandoc_syntax
    au! BufNewFile,BufFilePre,BufRead *.md set filetype=markdown.pandoc
augroup END

" do not add a newline if a file does not have it (this is some UNIX standard?)
" this shows files as modified in VCS even though I did not edit them (but vim
" just adds a newline). Also it fucks up big time OpenAPI's Mustache
" templates.
set nofixeol
