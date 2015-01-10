" Set up vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#begin()

" Plugin management
Plugin 'gmarik/vundle'

" Appearance stuff
Plugin 'bling/vim-airline'
Plugin 'chriskempson/base16-vim'
Plugin 'w0ng/vim-hybrid'
Plugin 'airblade/vim-gitgutter'
Plugin 'bling/vim-bufferline'
Plugin 'myusuf3/numbers.vim'

" File switching
Plugin 'kien/ctrlp.vim'
Plugin 'a.vim'

" Extra language support
Plugin 'fatih/vim-go'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'kchmck/vim-coffee-script'
Plugin 'Glench/Vim-Jinja2-Syntax'
Plugin 'groenewege/vim-less'
Plugin 'ap/vim-css-color'

" Lispy stuff
Plugin 'kien/rainbow_parentheses.vim'

" Gist magic
Plugin 'mattn/webapi-vim'
Plugin 'mattn/gist-vim'

" Autocomplete
Plugin 'Valloric/YouCompleteMe'

" Utils
Plugin 'scrooloose/nerdtree'
Plugin 'majutsushi/tagbar'

" Plugins to try out
Plugin 'mattn/emmet-vim'
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

" Actually make sure all plugins are loaded
call vundle#end()
