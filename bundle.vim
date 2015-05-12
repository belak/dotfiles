silent! if plug#begin('~/.vim/plugged')

" Appearance stuff
Plug 'bling/vim-airline'
Plug 'bling/vim-bufferline'
Plug 'myusuf3/numbers.vim'
Plug 'w0ng/vim-hybrid'

" File switching
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'Shougo/unite.vim'

" Extra language support
"Plug 'ap/vim-css-color'
Plug 'ekalinin/Dockerfile.vim'
Plug 'fatih/vim-go'
Plug 'Glench/Vim-Jinja2-Syntax'
"Plug 'groenewege/vim-less'
"Plug 'kchmck/vim-coffee-script'
"Plug 'mustache/vim-mustache-handlebars'
Plug 'pangloss/vim-javascript'
Plug 'rust-lang/rust.vim'

" Random
Plug 'junegunn/limelight.vim'
Plug 'junegunn/seoul256.vim'
Plug 'junegunn/vim-pseudocl'
Plug 'junegunn/vim-oblique'

" Utils
Plug 'ConradIrwin/vim-bracketed-paste'
"Plug 'kien/rainbow_parentheses.vim'
"Plug 'mattn/emmet-vim'
"Plug 'mileszs/ack.vim'
" Neocomplete is disabled until lua support is added in neovim
"Plug 'Shougo/neocomplete.vim'

" VCS
Plug 'gregsexton/gitv'
Plug 'mhinz/vim-signify'
Plug 'tpope/vim-fugitive'

" tpope!
"Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rsi'
"Plug 'tpope/vim-sleuth'
"Plug 'tpope/vim-surround'
"Plug 'tpope/vim-unimpaired'

" Actually make sure all plugins are loaded
call plug#end()
endif
