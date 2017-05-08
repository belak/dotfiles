call plug#begin()

" Simple tweaks
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-surround'
Plug 'airblade/vim-gitgutter'
Plug 'editorconfig/editorconfig-vim'
Plug 'myusuf3/numbers.vim'

" Extra utilities
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'tpope/vim-fugitive'

" Appearance
Plug 'bling/vim-bufferline'
Plug 'ntpeters/vim-airline-colornum'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'w0ng/vim-hybrid'

" Completion
function! DoRemote(arg)
    UpdateRemotePlugins
endfunction
Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote') }
Plug 'zchee/deoplete-go', { 'do': 'make'}
Plug 'zchee/deoplete-jedi'

" Linting and compiling
Plug 'benekastah/neomake'

" Language support
Plug 'fatih/vim-go'
Plug 'pearofducks/ansible-vim'

call plug#end()

let g:hybrid_custom_term_colors = 1
set background=dark
colorscheme hybrid

" Clear some stuff for vim-airline-colornum
set cursorline
hi clear CursorLine

" Deoplete settings
let g:deoplete#sources#go#align_class = 1
let g:deoplete#enable_at_startup = 1

" Airline settings
"
" Because I don't use powerline, removing the separators makes it look much
" better.
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_theme='hybridline'

" Enable mouse support
set mouse=a

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
set scrolloff=5       " Ensure we have a buffer of 5 lines at the top and bottom

" Random settings and such
set hidden                          " Allow buffer switching without saving
set iskeyword-=.                    " '.' is an end of word designator
set iskeyword-=#                    " '#' is an end of word designator
set iskeyword-=-                    " '-' is an end of word designator
set iskeyword-=_                    " '_' is an end of word designator

set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespace

" If clipboard is available, do everything we can to yank to the system
" clipboard rather than only the internal keyboard.
if has('clipboard')
    if has('unnamedplus')  " When possible use + register for copy-paste
        set clipboard=unnamed,unnamedplus
    else         " On mac and Windows, use * register for copy-paste
        set clipboard=unnamed
    endif
endif

" Set the leader key
let mapleader=","

" neomake mappings
nmap <leader><Space>o :lopen<CR>      " open location window
nmap <leader><Space>c :lclose<CR>     " close location window
nmap <leader><Space>, :ll<CR>         " go to current error/warning
nmap <leader><Space>n :lnext<CR>      " next error/warning
nmap <leader><Space>p :lprev<CR>      " previous error/warning

" Golang specific settings
let g:go_fmt_command = "goimports"

" Make python indentation saner
let g:pyindent_open_paren = '&sw'
let g:pyindent_nested_paren = '&sw'
let g:pyindent_continue = '&sw'

" Enable linting on file open and save
autocmd! BufWritePost,BufEnter * Neomake

" Make sure we don't auto-wrap all text, unless we're in a markdown file.
set formatoptions-=t
autocmd! FileType markdown setlocal formatoptions+=t

" Highlight the end of long lines in red
au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" Allow using the repeat operator with a visual selection (!)
" http://stackoverflow.com/a/8064607/127816
vnoremap . :normal .<CR>

" Make window switching easier
nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l

" Add a bind for fzf
nmap <C-p> :FZF<CR>

" Wrapped lines goes down/up to next row, rather than next line in file.
noremap j gj
noremap k gk

" Adjust viewports to the same size
map <Leader>= <C-w>=

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
