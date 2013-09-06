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

" Airline settings
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_theme='badwolf'

" Bundles
Bundle "gmarik/vundle"
Bundle "bling/vim-airline"
Bundle "myusuf3/numbers.vim"
Bundle "chriskempson/base16-vim"
Bundle "airblade/vim-gitgutter"
Bundle "kien/ctrlp.vim"
Bundle "bling/vim-bufferline"

" Convenience remappings
let g:clipbrdDefaultReg = '+'
let mapleader=","
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
set background=dark
let base16colorspace=256
colorscheme base16-tomorrow
filetype plugin indent on
syntax on

" Filetype specific stuff
au BufRead,BufNewFile *.md set filetype=markdown
au BufWritePre *.go Fmt
