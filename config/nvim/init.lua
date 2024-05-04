vim.opt.compatible = false

-- Variables {{{

HOME = os.getenv("HOME")

-- }}}

-- Plugins {{{

-- Normally we'd set the leader lower in this init file, but lazy.nvim
-- recommends setting it much earlier.
vim.g.mapleader = ","

-- bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Set up plugins
require("lazy").setup({
  -- Simple tweaks
  "tpope/vim-rsi",
  "tpope/vim-surround",

  -- Extra utilities
  "airblade/vim-gitgutter",
  "airblade/vim-rooter",
  "tpope/vim-commentary",
  "tpope/vim-fugitive",

  -- Appearance
  'myusuf3/numbers.vim',
  'nvim-lualine/lualine.nvim',
  'w0ng/vim-hybrid',
})

-- }}}

-- Appearance {{{

-- Colorscheme and syntax settings
vim.g.hybrid_custom_term_colors = 1
vim.opt.background = "dark"
vim.cmd([[colorscheme hybrid]])

require('lualine').setup({
  options = {
    theme = "16color",
    icons_enabled = false,
    section_separators = '',
    component_separators = '|',
  },
})

-- Clear some stuff for vim-airline-colornum
--vim.opt.cursorline = true
--vim.cmd([[hi clear CursorLine]])

-- Highlight VCS conflict markers
vim.cmd([[match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$']])

vim.opt.list = true
-- set listchars=tab:▸\ ,trail:•,extends:#,nbsp:.,eol:¬ " Highlight problematic whitespace

-- }}}

-- Various Settings {{{

-- We use rooter to automatically cd to the project root, but we don't want it to
-- tell us it did that.
vim.g.rooter_silent_chdir = 1

-- Default tab settings
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4

-- Random settings
vim.opt.autoindent  = true      -- Enable basic indentitation, smarter than "smart"indent
vim.opt.incsearch   = true      -- Start searching as we start typing
vim.opt.hlsearch    = true      -- Hilight what we're searching for
vim.opt.showcmd     = true      -- Always show the currently entered command
vim.opt.writebackup = true      -- Make a backup before overwriting a file
vim.opt.ttyfast     = true      -- Make vim more responsive
vim.opt.lazyredraw  = true      -- Don't show intermediate macro steps
vim.opt.smartcase   = true      -- Ignore case if search pattern is all lower case
vim.opt.autowrite   = true      -- Write when switching buffers
vim.opt.colorcolumn = "+81"     -- PEP-8 usefulness
vim.opt.virtualedit = "block"   -- Make moving in visual mode make more sense
vim.opt.synmaxcol   = 800       -- Don't try to highlight lines longer than 800 chars
vim.opt.textwidth   = 80        -- Auto wrap comments at 80 chars
vim.opt.hidden      = true      -- Allow buffer switching without saving
vim.opt.showmode    = false     -- No point since we use airline
vim.opt.scrolloff   = 5         -- Ensure we have a buffer of 5 lines at the top and bottom
vim.cmd([[
  set fillchars=vert:\│     " Unicode line for separators
  set iskeyword-=.          " '.' is an end of word designator
  set iskeyword-=#          " '#' is an end of word designator
  set iskeyword-=-          " '-' is an end of word designator
  set iskeyword-=_          " '_' is an end of word designator
]])

-- If clipboard is available, do everything we can to yank to the system
-- clipboard rather than only the internal keyboard.
--set clipboard^=unnamed
--if vim.fn.has('clipboard') == 1 then
--  if vim.fn.has('unnamedplus') == 1 then
--    -- When possible use + register for copy-paste
--    vim.opt.clipboard = "unnamed,unnamedplus"
--  else
--    -- On mac and Windows, use * register for copy-paste
--    vim.opt.clipboard = "unnamed"
--  end
--end

-- Split and select the right window
vim.opt.splitbelow = true
vim.opt.splitright = true

-- Line numbers
vim.opt.number = true
vim.opt.relativenumber = true

-- Make sure the annoying backup files, swap files, and undo files stay out of
-- the code directories.
vim.opt.backupdir = HOME .. "/.config/nvim/backup"
vim.opt.directory = HOME .. "/.config/nvim/swap"
vim.opt.undodir   = HOME .. "/.config/nvim/undo"
vim.opt.wildignore:append("*/vendor/**")

-- Highlight the end of long lines in red
--au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)

-- Make sure we don't auto-wrap all text, unless we're in a markdown file.
--set formatoptions-=t
--autocmd! FileType markdown setlocal formatoptions+=t

-- }}}

-- Keybinds {{{

-- Key binds

-- Easier split stuff
--nmap vs :vsplit<cr>
--nmap sp :split<cr>

-- Allow using the repeat operator with a visual selection (!)
-- http://stackoverflow.com/a/8064607/127816
--vnoremap . :normal .<CR>

-- Easier movement between panes
--nmap <C-h> <C-w>h
--nmap <C-j> <C-w>j
--nmap <C-k> <C-w>k
--nmap <C-l> <C-w>l

-- Wrapped lines goes down/up to next row, rather than next line in file.
--noremap j gj
--noremap k gk

-- Add a bind for fzf
--nmap <C-p> :GFiles<cr>

-- Clear search results
--nmap <leader>c :let @/=""<CR>
