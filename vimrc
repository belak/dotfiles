" Stuff for vundle
set nocompatible
filetype off

if filereadable(expand($HOME . "/.vim/vimrc.bundle"))
	source $HOME/.vim/vimrc.bundle
endif

" Remove some gui stuff
set guifont=Terminus\ 8
set guioptions-=T
set guioptions-=r
set guioptions-=m
set go-=L

" Airline settings
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_theme='base16'

" Reverse the order of CtrlP
let g:ctrlp_match_window = 'bottom,order:ttb,min:1,max:5,results:5'

" Remove some gui stuff
set guifont=Terminus\ 8
set guioptions-=T
set guioptions-=r
set guioptions-=m
set go-=L

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
set lazyredraw                 " Don't show intermediate macro steps
set smartcase                  " Ignore case if search pattern is all lower case
set autowrite                  " Write when switching buffers
set colorcolumn=80             " PEP-8 usefulness
set autoread                   " Auto re-read files when changed outside vim

" Splits
set splitbelow
set splitright

" Quicker exit of insert mode
set ttimeout
set ttimeoutlen=0

set mouse=a

" Line numbers
" This is required for numbers.vim
set number
set rnu

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

" Show whitespace
set listchars=tab:▸\ ,eol:¬
nmap <Leader>l :set list!<CR>

" Random bindings
"nmap <leader>a :A<CR>
nmap <leader>a :Ack
nmap <leader>d :CtrlPBuffer<cr>
nmap <C-b> :NERDTreeToggle<cr>
nmap <leader>lr :e app/routes.php<cr>

" Auto change directory to match current file ,cd
nnoremap <leader>cd :cd %:p:h<CR>:pwd<CR>

" Brief crosshairs on the cursor
function! CursorPing()
	set cursorline cursorcolumn
	redraw
	sleep 500m
	set nocursorline nocursorcolumn
endfunction
nmap <leader>f :call CursorPing()<CR>

" Filetype specific stuff
au BufRead,BufNewFile *.md setlocal filetype=markdown
au BufRead,BufNewFile *.py setlocal ts=8 et sw=4 ts=4
