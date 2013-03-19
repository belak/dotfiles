" Stuff for vundle
set nocompatible
filetype off

" Set up vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Enable go plugins
if exists("$GOROOT")
  set rtp+=$GOROOT/misc/vim/
endif

" Vundles
" vundle
Bundle "gmarik/vundle"

" Powerline
Bundle "Lokaltog/vim-powerline"
set ttimeout ttimeoutlen=0
"let g:Powerline_symbols = 'unicode'

" Switches number style based on current mode
Bundle "myusuf3/numbers.vim"

" Zenburn
Bundle "jnurmine/Zenburn"

" Convenience remappings
let g:clipbrdDefaultReg = '+'
let mapleader=","
nnoremap ; :
set pastetoggle=<F2>

" Random settings
set mouse=a
set number
set tabstop=4
set shiftwidth=4
set backspace=indent,eol,start
set incsearch
set hlsearch
set nobackup
set showcmd
set nobackup
set writebackup
set dir=~/.vim/swap
set laststatus=2

" Syntax stuff
colorscheme zenburn
filetype plugin indent on
syntax on

" Funky filetypes
au BufRead,BufNewFile *.md set filetype=markdown

