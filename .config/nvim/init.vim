" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.local/share/nvim/plugged')
Plug 'ctrlpvim/ctrlp.vim'

" elixir
Plug 'elixir-editors/vim-elixir'
" Plug 'amiralies/coc-elixir'  -- removed, use :CocInstall coc-elixir

Plug 'scrooloose/nerdtree'
" Plug 'sheerun/vim-polyglot'
Plug 'nanotech/jellybeans.vim'
Plug 'jceb/vim-orgmode'
" Plug 'nathanaelkane/vim-indent-guides'
Plug 'Yggdroot/indentLine'
" Plug 'universal-ctags/ctags'
Plug 'fatih/vim-go', {'do': ':GoInstallBinaries'}

Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

" Search
" Plug 'junegunn/fzf'
Plug 'jremmen/vim-ripgrep'

" Use release branch
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" " Or latest tag
" Plug 'neoclide/coc.nvim', {'tag': '*', 'branch': 'release'}
" " Or build from source code by use yarn: https://yarnpkg.com
" Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}

Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'

Plug 'neovimhaskell/haskell-vim'

" Git support
Plug 'tpope/vim-fugitive'
" Git gutter
Plug 'airblade/vim-gitgutter'

Plug 'ryanoasis/vim-devicons'
Plug 'chrisbra/Colorizer'
Plug 'luochen1990/rainbow'

Plug 'fatih/vim-go', {'do': ':GoInstallBinaries'}

Plug 'stephpy/vim-yaml', { 'for': 'yaml' }

Plug 'elmcast/elm-vim'

" colour schemes
" Plug 'flrnprz/plastic'
" Plug 'rafi/awesome-vim-colorschemes'
" Plug 'joshdick/onedark.vim'
" Plug 'KeitaNakamura/neodark.vim'
" Plug 'rakr/vim-one'
" Plug 'flrnprz/candid.vim'
Plug 'ayu-theme/ayu-vim'
Plug 'NLKNguyen/papercolor-theme'

" Inatialize plugin system
call plug#end()





filetype plugin indent on
set clipboard=unnamedplus
set number
set relativenumber
set splitright
set nowrap

" persistent
set undodir=~/.cache/vimundo/
set undofile
set backupdir=~/.cache/nvim_cache
set directory=~/.cache/nvim_cache

nnoremap ]e :lnext <CR>
nnoremap [e :lprev <CR>
nnoremap ]lo :lopen <CR>
set laststatus=2

if (has("termguicolors"))
 set termguicolors
endif

set background=dark

let g:PaperColor_Theme_Options = {
  \   'theme': {
  \     'default.dark': {
  \       'override' : {
  \         'color00' : ['#000000', '0'],
  \       }
  \     }
  \   }
  \ }

" colorscheme PaperColor

" let ayucolor="dark"   " for dark version of theme
" colorscheme ayu

"colorscheme plastic

" let g:neodark#background = '#000000'
" colorscheme neodark

"let g:onedark_color_overrides = {
"\ "black": {"gui": "#000000", "cterm": "0", "cterm16": "0" },
"\}
"colorscheme onedark


set expandtab
set shiftwidth=2
set softtabstop=0
set tabstop=2
set title

let NERDTreeShowHidden=1
set updatetime=100


" Rainbow
let g:rainbow_active = 1

inoremap jk <Esc>

let mapleader = " "
let maplocalleader = "\\"

" Required for operations modifying multiple buffers like rename.
set hidden


let g:LanguageClient_serverCommands = {
    \ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
    \ 'javascript': ['/usr/local/bin/javascript-typescript-stdio'],
    \ 'javascript.jsx': ['tcp://127.0.0.1:2089'],
    \ 'python': ['/usr/local/bin/pyls'],
    \ 'haskell': ['ghcide', '--lsp'],
    \ 'ruby': ['~/.rbenv/shims/solargraph', 'stdio'],
    \ }


au User lsp_setup call lsp#register_server({
    \ 'name': 'ghcide',
    \ 'cmd': {server_info->['/home/andre/.local/bin/ghcide', '--lsp']},
    \ 'whitelist': ['haskell'],
    \ })


" Ctrl-P settings
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP' " files, buffers and MRU
let g:ctrlp_working_path_mode = 'ra'  " root is .git or current file if not .git

" ------------------------------------------------------------------------------------------
" coc
" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

  " Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

  " Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
  " Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
  " Or use `complete_info` if your vim support it, like:
  " inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
  "
" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')
" ------------------------------------------------------------------------------------------



nnoremap <F5> :call LanguageClient_contextMenu()<CR>
" Or map each action separately
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
nnoremap <F10> :buffers<CR>:buffer<Space>
nmap <F8> :NERDTreeToggle<CR>
nnoremap <CR> :noh<CR><CR>

colorscheme PaperColor
