" Stuff for vundle
set nocompatible

" Incsearch
set incsearch

" Convenience remappings
set clipboard^=unnamed
let mapleader=","
set pastetoggle=<F2>

" Tab settings
set tabstop=4
set shiftwidth=4

" Random settings
set backspace=indent,eol,start " Allow backspacing over everything in insert mode
set hlsearch                   " Hilight what we're searching for
set showcmd                    " Always show the currently entered command
set writebackup                " Make a backup before overwriting a file
set ttyfast                    " Make vim more responsive
set lazyredraw                 " Don't show intermediate macro steps
set smartcase                  " Ignore case if search pattern is all lower case
set autowrite                  " Write when switching buffers
set colorcolumn=80             " PEP-8 usefulness
set autoread                   " Auto re-read files when changed outside vim
set virtualedit=block          " Make moving in visual mode make more sense
set synmaxcol=800              " Don't try to highlight lines longer than 800 chars
set textwidth=80               " Auto wrap comments at 80 chars
set fillchars=vert:\│          " Unicode line for separators
set ruler                      " Column and line num display

" Small things for gvim
if has('gui_running')
	set guioptions-=T
	set guioptions-=L
	set guioptions-=m
	set guioptions-=r
endif

" Split and select the right window
set splitbelow
set splitright

" Quicker exit of insert mode
set ttimeout
set ttimeoutlen=0

" Turn on mouse support and make it work past the normal xterm limit
set mouse=a
if has('mouse_sgr')
	set ttymouse=sgr
endif

" Line numbers
set number
set relativenumber

" Make sure the annoying backup files, swap files, and undo files stay out of
" the code directories.
set backupdir=~/.vim/backup
set directory=~/.vim/swap
set undodir=~/.vim/undo
set wildignore+=*/vendor/**

" Syntax highlighting
set background=dark
colorscheme pablo
syntax on
filetype plugin indent on

" Easier split stuff
nmap vs :vsplit<cr>
nmap sp :split<cr>

" Easier movement between panes
nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l

" Show whitespace
set listchars=tab:▸\ ,eol:¬
nmap <silent> <leader>w :set list!<CR>

" Clear search results
nmap <leader>c :let @/=""<CR>

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" Change the cursor shape in insert mode for iTerm2, even inside tmux
if exists('$TMUX')
	let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
	let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
elseif &term =~ "xterm\\|rxvt"
	let &t_SI = "\<Esc>[6 q"
	let &t_EI = "\<Esc>[2 q"
else
	let &t_SI = "\<Esc>]50;CursorShape=1\x7"
	let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

" Filetype specific stuff
au BufRead,BufNewFile *.md setlocal filetype=markdown
au BufRead,BufNewFile *.sp setlocal filetype=cpp
au BufRead,BufNewFile *.py setlocal expandtab shiftwidth=4 tabstop=4
