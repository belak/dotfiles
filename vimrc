set nocompatible

" Plugins {{{
call plug#begin()

Plug 'tpope/vim-sensible'

" Simple tweaks
Plug 'airblade/vim-gitgutter'
Plug 'editorconfig/editorconfig-vim'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-surround'

" Extra utilities
Plug 'airblade/vim-rooter'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'preservim/nerdtree'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'

" Appearance
Plug 'bling/vim-bufferline'
Plug 'itchyny/lightline.vim'
Plug 'myusuf3/numbers.vim'
Plug 'w0ng/vim-hybrid'

" Linting and compiling
Plug 'dense-analysis/ale'

" Language support
Plug 'cespare/vim-toml'
Plug 'ekalinin/Dockerfile.vim'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'hashivim/vim-terraform'
Plug 'leafgarland/typescript-vim'
Plug 'mxw/vim-jsx'
Plug 'NoahTheDuke/vim-just'
Plug 'pangloss/vim-javascript'
Plug 'pearofducks/ansible-vim'
Plug 'rust-lang/rust.vim'
Plug 'posva/vim-vue'
Plug 'uarun/vim-protobuf'
Plug 'zorab47/procfile.vim'

call plug#end()
" }}}

" Appearance {{{

" Colorscheme and syntax settings
let g:hybrid_custom_term_colors = 1
set background=dark
colorscheme hybrid

" Clear some stuff for vim-airline-colornum
set cursorline
hi clear CursorLine

" Small tweaks to hide UI elements in gvim
if has('gui_running')
  set guioptions-=T
  set guioptions-=L
  set guioptions-=m
  set guioptions-=r
endif

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

set list
"set listchars=tab:▸\ ,trail:•,extends:#,nbsp:.,eol:¬ " Highlight problematic whitespace

" Lightline related config
let g:lightline = {
      \ 'colorscheme': 'default',
      \ }

" }}}

" Various Settings {{{

" We use rooter to automatically cd to the project root, but we don't want it to
" tell us it did that.
let g:rooter_silent_chdir = 1

" Golang specific settings
let g:go_fmt_command = "goimports"

" Make python indentation saner
let g:pyindent_open_paren = '&sw'
let g:pyindent_nested_paren = '&sw'
let g:pyindent_continue = '&sw'

set incsearch

" Convenience remappings
set clipboard^=unnamed
let mapleader=","
set pastetoggle=<F2>

" Tab settings
set tabstop=4
set shiftwidth=4

" Random settings
set autoindent                 " Enable basic indentitation, smarter than "smart"indent
set hlsearch                   " Hilight what we're searching for
set showcmd                    " Always show the currently entered command
set writebackup                " Make a backup before overwriting a file
set ttyfast                    " Make vim more responsive
set lazyredraw                 " Don't show intermediate macro steps
set smartcase                  " Ignore case if search pattern is all lower case
set autowrite                  " Write when switching buffers
set colorcolumn=81             " PEP-8 usefulness
set virtualedit=block          " Make moving in visual mode make more sense
set synmaxcol=800              " Don't try to highlight lines longer than 800 chars
set textwidth=80               " Auto wrap comments at 80 chars
set fillchars=vert:\│          " Unicode line for separators
set hidden                     " Allow buffer switching without saving
set iskeyword-=.               " '.' is an end of word designator
set iskeyword-=#               " '#' is an end of word designator
set iskeyword-=-               " '-' is an end of word designator
set iskeyword-=_               " '_' is an end of word designator
set noshowmode                 " No point since we use airline
set scrolloff=5                " Ensure we have a buffer of 5 lines at the top and bottom

" If clipboard is available, do everything we can to yank to the system
" clipboard rather than only the internal keyboard.
if has('clipboard')
  if has('unnamedplus')  " When possible use + register for copy-paste
    set clipboard=unnamed,unnamedplus
  else                   " On mac and Windows, use * register for copy-paste
    set clipboard=unnamed
  endif
endif

" Split and select the right window
set splitbelow
set splitright

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

" Highlight the end of long lines in red
"au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)

" Change the cursor shape in insert mode for iTerm2, even inside tmux
if exists('$TMUX')
  let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>[6 q\<Esc>\\"
  let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>[2 q\<Esc>\\"
elseif &term =~ "xterm\\|rxvt"
  let &t_SI = "\<Esc>[6 q"
  let &t_EI = "\<Esc>[2 q"
else
  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

" Filetype specific stuff
au BufRead,BufNewFile *.tsx setlocal filetype=typescript.jsx
au BufRead,BufNewFile *.md setlocal filetype=markdown
au BufRead,BufNewFile *.sp setlocal filetype=cpp

" Make sure we don't auto-wrap all text, unless we're in a markdown file.
set formatoptions-=t
autocmd! FileType markdown setlocal formatoptions+=t

" }}}

" Keybinds {{{

""" Key binds

" neomake mappings
nmap <leader><Space>o :lopen<CR>      " open location window
nmap <leader><Space>c :lclose<CR>     " close location window
nmap <leader><Space>, :ll<CR>         " go to current error/warning
nmap <leader><Space>n :lnext<CR>      " next error/warning
nmap <leader><Space>p :lprev<CR>      " previous error/warning

" Easier split stuff
nmap vs :vsplit<cr>
nmap sp :split<cr>

" Allow using the repeat operator with a visual selection (!)
" http://stackoverflow.com/a/8064607/127816
vnoremap . :normal .<CR>

" Easier movement between panes
nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l

" Wrapped lines goes down/up to next row, rather than next line in file.
noremap j gj
noremap k gk

" Add a bind for fzf
nmap <C-p> :GFiles<cr>

" Clear search results
nmap <leader>c :let @/=""<CR>

" Adjust viewports to the same size
map <Leader>= <C-w>=

nmap <Tab> za

" Fugitive settings
nnoremap <silent> <leader>gs :Gstatus<CR>
nnoremap <silent> <leader>gd :Gdiff<CR>
nnoremap <silent> <leader>gc :Gcommit<CR>
nnoremap <silent> <leader>gb :Gblame<CR>
nnoremap <silent> <leader>gl :Glog<CR>
nnoremap <silent> <leader>gp :Git push<CR>
nnoremap <silent> <leader>gr :Gread<CR>
nnoremap <silent> <leader>gw :Gwrite<CR>
nnoremap <silent> <leader>ge :Gedit<CR>
" Mnemonic _i_nteractive
nnoremap <silent> <leader>gi :Git add -p %<CR>
nnoremap <silent> <leader>gg :SignifyToggle<CR>

" Map the scroll wheel to go a single line at a time
"map <ScrollWheelUp> <C-Y>
"map <ScrollWheelDown> <C-E>

" }}}

" Unused Tweaks {{{
"
" An alternative to colorcolumn=81, this only highlights on lines where we
" actually go past 80 characters.
"
"exe "hilight ColorColumn" .s:fg_none .s:bg_red .fmt_none
"call matchadd('LongLineWarning', '\%81v', 100)
"call matchadd('ColorColumn', '\%81v', 100)
"
" : is used more often than ;
"nnoremap ; :
"nnoremap : ;
"
" }}}
