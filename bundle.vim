call plug#begin('~/.vim/plugged')

" Appearance stuff
Plug 'mhinz/vim-signify'
Plug 'bling/vim-airline'
Plug 'bling/vim-bufferline'
Plug 'myusuf3/numbers.vim'
Plug 'w0ng/vim-hybrid'

" File switching
Plug 'kien/ctrlp.vim'

" Extra language support
Plug 'ap/vim-css-color'
Plug 'fatih/vim-go'
Plug 'Glench/Vim-Jinja2-Syntax'
Plug 'groenewege/vim-less'
Plug 'kchmck/vim-coffee-script'
Plug 'mustache/vim-mustache-handlebars'
Plug 'wting/rust.vim'

" Utils
Plug 'kien/rainbow_parentheses.vim'
Plug 'mattn/emmet-vim'
Plug 'mileszs/ack.vim'
Plug 'scrooloose/nerdcommenter'
" Neocomplete is disabled until lua support is added in neovim
"Plug 'Shougo/neocomplete.vim'

" tpope!
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'

" Actually make sure all plugins are loaded
call plug#end()
