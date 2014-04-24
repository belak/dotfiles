" Stuff for vundle
set nocompatible
filetype off

" Remove some gui stuff
set guifont=Terminus\ 8
set guioptions-=T
set guioptions-=r
set guioptions-=m
set go-=L

" Set up vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Airline settings
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_theme='badwolf'

" Bundle management
Bundle 'gmarik/vundle'

" Appearance stuff
Bundle 'bling/vim-airline'
Bundle 'chriskempson/base16-vim'
Bundle 'airblade/vim-gitgutter'
Bundle 'bling/vim-bufferline'
Bundle 'myusuf3/numbers.vim'

" File switching
Bundle 'kien/ctrlp.vim'
Bundle 'a.vim'

" Completion
Bundle 'Valloric/YouCompleteMe'

" Golang stuff
Bundle 'fatih/vim-go'

" Lispy stuff
Bundle 'kien/rainbow_parentheses.vim'

" Gist magic
Bundle 'mattn/webapi-vim'
Bundle 'mattn/gist-vim'

" Plugins to try out
Bundle 'mattn/emmet-vim'
Bundle 'scrooloose/nerdtree'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'tpope/vim-surround'
Bundle 'mileszs/ack.vim'
Bundle 'scrooloose/nerdcommenter'
Bundle 'tpope/vim-unimpaired'
Bundle 'ervandew/supertab'
Bundle 'tpope/vim-fugitive'
Bundle 'jeetsukumaran/vim-buffergator'
Bundle 'ZoomWin'
Bundle 'sophacles/vim-bundle-mako'

" Convenience remappings
let g:clipbrdDefaultReg = '+'
let mapleader=","
set pastetoggle=<F2>

" Tab settings
set tabstop=4
set shiftwidth=4

" Random settings
set backspace=indent,eol,start " Allow backspacing over everything in insert mode
set incsearch                  " Search while typing
set hlsearch                   " Hilight what we're searching for
set showcmd                    " Always show the currently entered command
set writebackup                " Make a backup before overwriting a file
set laststatus=2               " Always show the status line
set ttyfast                    " Make vim more responsive
set smartcase                  " Ignore case if search pattern is all lower case
set autowrite                  " Write when switching buffers

set mouse=a

" Line numbers
" This is required for numbers.vim
set number

set backupdir=~/.vim/backup
set directory=~/.vim/swap
set wildignore+=*/vendor/**

" Syntax stuff
set background=dark
let base16colorspace=256
colorscheme base16-ocean
filetype plugin indent on
syntax on

" Remove search results
command! C let @/=""

" Easier split stuff
nmap vs :vsplit<cr>
nmap sp :split<cr>

nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l

" Random bindings
nmap <leader>a :A<CR>
nmap <C-b> :NERDTreeToggle<cr>
nmap <leader>lr :e app/routes.php<cr>

" Auto change directory to match current file ,cd
nnoremap <leader>cd :cd %:p:h<CR>:pwd<CR>

" Filetype specific stuff
au BufRead,BufNewFile *.md set filetype=markdown
au BufRead,BufNewFile *.py set ts=8 et sw=4 ts=4
