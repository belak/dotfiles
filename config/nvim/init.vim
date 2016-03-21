call plug#begin()

" Simple tweaks
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-surround'
Plug 'airblade/vim-gitgutter'
Plug 'myusuf3/numbers.vim'

" Extra utilities
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'tpope/vim-fugitive'

" Appearance
Plug 'bling/vim-bufferline'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'w0ng/vim-hybrid'

" Language support
Plug 'fatih/vim-go'
call plug#end()

let g:hybrid_custom_term_colors = 1
set background=dark
colorscheme hybrid
filetype plugin indent on

" Airline settings
"
" Because I don't use powerline, removing the separators makes it look much
" better.
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_theme='hybridline'

" Make splitting more intuitive
set splitbelow
set splitright

" Having both number and relativenumber set will display relative line numbers
" on most lines and the absolute number on the current line.
set number
set relativenumber

" Indentation and line wrapping
"
" Note: I mostly use python, so this defaults to those settings even though I
" generally prefer to use hard tabs and not worry about the line width.
set tabstop=4
set shiftwidth=4
set expandtab
set colorcolumn=80
set textwidth=80

set autoindent        " Turn on autoindent
set noshowmode        " No point since we use airline
set fillchars=vert:\│ " Unicode line for separators
set lazyredraw        " Redraw less when running macros

" Golang specific settings
let g:go_fmt_command = "goimports"

" Make python indentation saner
let g:pyindent_open_paren = '&sw'
let g:pyindent_nested_paren = '&sw'
let g:pyindent_continue = '&sw'

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l
