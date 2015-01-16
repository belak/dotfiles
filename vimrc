" Stuff for vundle
set nocompatible

if filereadable(expand($HOME . "/.bundle.vim"))
	source $HOME/.bundle.vim
endif

" Remove some gui stuff
if has("gui_running")
	set guifont=Menlo\ 12
	set guioptions-=l
	set guioptions-=L
	set guioptions-=m
	set guioptions-=M
	set guioptions-=r
	set guioptions-=R
	set guioptions-=T
endif

" Airline settings
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_theme='base16'

" Bufferline
let g:bufferline_echo = 0

" Reverse the order of CtrlP
let g:ctrlp_match_window = 'bottom,order:ttb,min:1,max:5,results:5'

" Use silver searcher for ack
if executable('ag')
	let g:ackprg = 'ag --nogroup --nocolor --column'
	let grepprg = 'ag --nogroup --nocolor --column'

	" Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
	let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif

" Completion
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete

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
"set colorcolumn=80             " PEP-8 usefulness
set autoread                   " Auto re-read files when changed outside vim
set virtualedit=block          " Make moving in visual mode make more sense
set synmaxcol=800              " Don't try to highlight lines longer than 800 chars
set textwidth=72               " Auto wrap comments at 72 chars

" Splits
set splitbelow
set splitright

" Quicker exit of insert mode
set ttimeout
set ttimeoutlen=0

set mouse=a
if has('mouse_sgr')
	set ttymouse=sgr
endif

" Line numbers
" This is required for numbers.vim
set number
set rnu

set backupdir=~/.vim/backup
set directory=~/.vim/swap
set wildignore+=*/vendor/**

" Syntax stuff
set background=dark
colorscheme hybrid

" This was taken from vim-gitgutter
function! GetBackgroundColors(group)
	redir => highlight
	silent execute 'silent highlight ' . a:group
	redir END

	let link_matches = matchlist(highlight, 'links to \(\S\+\)')
	if len(link_matches) > 0 " follow the link
		return GetBackgroundColors(link_matches[1])
	endif

	let ctermbg = MatchHighlight(highlight, 'ctermbg=\([0-9A-Za-z]\+\)')
	let guibg   = MatchHighlight(highlight, 'guibg=\([#0-9A-Za-z]\+\)')
	return [guibg, ctermbg]
endfunction

" So was this
function! MatchHighlight(highlight, pattern)
  let matches = matchlist(a:highlight, a:pattern)
  if len(matches) == 0
    return 'NONE'
  endif
  return matches[1]
endfunction

let [guibg, ctermbg] = GetBackgroundColors('SignColumn')

let g:signify_sign_modify = '~'
let g:signify_cursorhold_insert = 1
let g:signify_cursorhold_normal = 1
execute "highlight SignifySignAdd    ctermbg=" . ctermbg . " ctermfg=2 cterm=bold"
execute "highlight SignifySignDelete ctermbg=" . ctermbg . " ctermfg=1 cterm=bold"
execute "highlight SignifySignChange ctermbg=" . ctermbg . " ctermfg=3"

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
nmap <leader>a :Ack<space>
nmap <leader>d :CtrlPBuffer<cr>
nmap <C-b> :NERDTreeToggle<cr>
nmap <F8> :TagbarToggle<CR>

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

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

if exists('$TMUX')
	let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
	let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
	let &t_SI = "\<Esc>]50;CursorShape=1\x7"
	let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

" Filetype specific stuff
au BufRead,BufNewFile *.md setlocal filetype=markdown
au BufRead,BufNewFile *.py setlocal ts=8 et sw=4 ts=4
