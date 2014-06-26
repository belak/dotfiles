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

" Plugin management
Plugin 'gmarik/vundle'

" Appearance stuff
Plugin 'bling/vim-airline'
Plugin 'chriskempson/base16-vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'bling/vim-bufferline'
Plugin 'myusuf3/numbers.vim'

" File switching
Plugin 'kien/ctrlp.vim'
Plugin 'a.vim'

" Completion
Plugin 'Valloric/YouCompleteMe'

" Extra language support
Plugin 'fatih/vim-go'
Plugin 'mustache/vim-mustache-handlebars'

" Lispy stuff
Plugin 'kien/rainbow_parentheses.vim'

" Gist magic
Plugin 'mattn/webapi-vim'
Plugin 'mattn/gist-vim'

" Plugins to try out
Plugin 'mattn/emmet-vim'
Plugin 'scrooloose/nerdtree'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'tpope/vim-surround'
Plugin 'mileszs/ack.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'tpope/vim-unimpaired'
Plugin 'ervandew/supertab'
Plugin 'tpope/vim-fugitive'
Plugin 'jeetsukumaran/vim-buffergator'
Plugin 'ZoomWin'
Plugin 'sophacles/vim-bundle-mako'

" Convenience remappings
set clipboard^=unnamed
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
colorscheme base16-tomorrow
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
