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
" Plug 'Yggdroot/indentLine'
" Plug 'universal-ctags/ctags'
Plug 'fatih/vim-go', {'do': ':GoInstallBinaries'}

" Search
" Plug 'junegunn/fzf'
Plug 'jremmen/vim-ripgrep'

"haskell lsp
Plug 'neovimhaskell/haskell-vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

""haskell basic, intero etc
"Plug 'neovimhaskell/haskell-vim'
"Plug 'parsonsmatt/intero-neovim'
"Plug 'w0rp/ale'
"Plug 'neomake/neomake'


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

" When searching try to be smart about cases
set ignorecase
set smartcase

" Highlight search results
set hlsearch

" highlights searches as you go.
set incsearch

" Change cwd to current file dir
"set autochdir

" Ignore certain files with globbing
set wildignore+=*.zip,*.pyc,*.tar,*.gz
set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=.stack-work,dist-newstyle        " Haskell
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
set wildignore+=*.sw?                            " Vim swap files
set wildignore+=*.DS_Store                       " OSX 

" persistent
set undodir=~/.cache/vimundo/
set undofile
set backupdir=~/.cache/nvim_cache
set directory=~/.cache/nvim_cache

" Don't try to highlight long lines.
" This fixes some performance problems on huge files.
set synmaxcol=800

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


" Use spaces instead of tabs
set expandtab

" Be smart when using tabs
set smarttab

" 1 tab = 2 spaces
set shiftwidth=2
set tabstop=2
set softtabstop=2

" Show trailing whitespace
set list
set listchars=tab:»·,trail:·

" Sets the terminal title nicely.
set title

" No annoying sound on errors
set visualbell t_vb=

" Allow block selection over non-existant text
set virtualedit=block

" Large history
set history=500

" Don't redraw while executing macros (good for performance)
set lazyredraw

" Show commands-in-progress in status bar.
set showcmd

" Don't show startup screen
set shortmess+=Ic

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


"let g:intero_backend = {
"        \ 'command': 'stack ghci',
"        \ 'cwd': expand('%:p:h'),
"        \}
"
"let g:ale_linters = {'haskell': ['hlint', 'ghc']}
"let g:ale_haskell_ghc_options = '-fno-code -v0 -isrc'
"
"
"augroup interoMaps
"  au!
"  " Maps for intero. Restrict to Haskell buffers so the bindings don't collide.
"
"  " Background process and window management
"  au FileType haskell nnoremap <silent> <leader>is :InteroStart<CR>
"  au FileType haskell nnoremap <silent> <leader>ik :InteroKill<CR>
"
"  " Open intero/GHCi split horizontally
"  au FileType haskell nnoremap <silent> <leader>io :InteroOpen<CR>
"  " Open intero/GHCi split vertically
"  au FileType haskell nnoremap <silent> <leader>iov :InteroOpen<CR><C-W>H
"  au FileType haskell nnoremap <silent> <leader>ih :InteroHide<CR>
"
"  " Reloading (pick one)
"  " Automatically reload on save
"  au BufWritePost *.hs InteroReload
"  " Manually save and reload
"  ""au FileType haskell nnoremap <silent> <leader>wr :w \| :InteroReload<CR>
"
"  " Load individual modules
"  au FileType haskell nnoremap <silent> <leader>il :InteroLoadCurrentModule<CR>
"  au FileType haskell nnoremap <silent> <leader>if :InteroLoadCurrentFile<CR>
"
"  " Type-related information
"  " Heads up! These next two differ from the rest.
"  au FileType haskell map <silent> <leader>t <Plug>InteroGenericType
"  au FileType haskell map <silent> <leader>T <Plug>InteroType
"  au FileType haskell nnoremap <silent> <leader>it :InteroTypeInsert<CR>
"
"  " Navigation
"  au FileType haskell nnoremap <silent> <leader>jd :InteroGoToDef<CR>
"
"  " Managing targets
"  " Prompts you to enter targets (no silent):
"  au FileType haskell nnoremap <leader>ist :InteroSetTargets<SPACE>
"augroup END

" Sets the intero window to split vertically; default is horizontal
"let g:intero_vertical_split = 1
"let g:intero_start_immediately = 1

"
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


"haskell
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
"" Or map each action separately
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>


nnoremap <F10> :buffers<CR>:buffer<Space>
nmap <F8> :NERDTreeToggle<CR>
nnoremap <CR> :noh<CR><CR>

colorscheme PaperColor


" Allow C-w in terminals
tnoremap <C-w>h <C-\><C-n><C-w>h
tnoremap <C-w>j <C-\><C-n><C-w>j
tnoremap <C-w>k <C-\><C-n><C-w>k
tnoremap <C-w>l <C-\><C-n><C-w>l


" Insert mode by default for terminals
" https://github.com/neovim/neovim/issues/8816#issuecomment-539224440
let g:previous_window = -1
function SmartInsert()
  if &buftype == 'terminal'
    if g:previous_window != winnr()
      startinsert
    endif
    let g:previous_window = winnr()
  else
    let g:previous_window = -1
  endif
endfunction

au BufEnter * call SmartInsert()
