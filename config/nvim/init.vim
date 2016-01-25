call plug#begin()
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rsi'
Plug 'bling/vim-bufferline'
Plug 'vim-airline/vim-airline'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'w0ng/vim-hybrid'
call plug#end()

set background=dark
colorscheme hybrid
filetype plugin indent on

let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_theme='base16'

set splitbelow
set splitright

set number
set relativenumber

set tabstop=4
set shiftwidth=4
set colorcolumn=80
set textwidth=80

set fillchars=vert:\│          " Unicode line for separators
set lazyredraw
