vim.opt.compatible = false

-- Variables {{{

local HOME = os.getenv("HOME")

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

  -- Appearance
  "miikanissi/modus-themes.nvim",

  -- Various mini.nvim config
  {
    'echasnovski/mini.nvim',
    config = function()
      require('mini.comment').setup()

      require('mini.diff').setup()

      require('mini.git').setup()

      require('mini.indentscope').setup({
        draw = {
          delay = 0,
          animation = require('mini.indentscope').gen_animation.none(),
        }
      })

      require('mini.misc').setup()
      MiniMisc.setup_auto_root()

      require('mini.pick').setup()
      vim.keymap.set('n', '<leader>ff', MiniPick.builtin.files, {})
      vim.keymap.set('n', '<leader>fg', MiniPick.builtin.grep_live, {})
      vim.keymap.set('n', '<leader>fb', MiniPick.builtin.buffers, {})
      vim.keymap.set('n', '<leader>fh', MiniPick.builtin.help, {})

      require('mini.statusline').setup({
        use_icons = false,
      })

      require('mini.surround').setup()
    end
  },
})

-- }}}

-- Appearance {{{

-- Colorscheme and syntax settings
vim.opt.background = "dark"
vim.cmd([[colorscheme modus]])

-- Highlight VCS conflict markers
vim.cmd([[match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$']])

-- }}}

-- Various Settings {{{

-- Default tab settings
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4

-- Random settings
vim.opt.autoindent  = true      -- Enable basic indentation, smarter than "smart"indent
vim.opt.cursorline  = true      -- Highlight the current line
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
  set iskeyword-=.          " '.' is an end of word designator
  set iskeyword-=#          " '#' is an end of word designator
  set iskeyword-=-          " '-' is an end of word designator
  set iskeyword-=_          " '_' is an end of word designator
]])

-- If clipboard is available, do everything we can to yank to the system
-- clipboard rather than only the internal keyboard.
if vim.fn.has('clipboard') == 1 then
  if vim.fn.has('unnamedplus') == 1 then
    -- When possible use + register for copy-paste
    vim.opt.clipboard = "unnamedplus"
  else
    -- On mac and Windows, use * register for copy-paste
    vim.opt.clipboard = "unnamed"
  end
end

-- Split and select the right window
vim.opt.splitbelow = true
vim.opt.splitright = true

-- Line numbers
--
-- This includes some magic to disable relative numbers in insert mode and
-- disable absolute numbers in visual mode. It is roughly based on the
-- functionality of numbers.vim.
vim.opt.number = true
vim.opt.relativenumber = true

vim.api.nvim_create_autocmd('ModeChanged', {
  callback = function(event)
    vim.opt.number = true
    vim.opt.relativenumber = true

    if string.match(event.match, ":i$") then
      vim.opt.relativenumber = false
    elseif string.match(event.match, ":v$") then
      vim.opt.number = false
    end
  end
})

-- Make sure the annoying backup files, swap files, and undo files stay out of
-- the code directories.
vim.opt.backupdir = HOME .. "/.config/nvim/backup"
vim.opt.directory = HOME .. "/.config/nvim/swap"
vim.opt.undodir   = HOME .. "/.config/nvim/undo"
vim.opt.wildignore:append("*/vendor/**")

-- Make sure we don't auto-wrap all text, unless we're in a markdown file.
--set formatoptions-=t
--autocmd! FileType markdown setlocal formatoptions+=t

-- }}}

-- Keybinds {{{

-- Key binds

-- Easier split stuff
vim.keymap.set("n", "vs", ":vsplit<cr>")
vim.keymap.set("n", "sp", ":split<cr>")

-- Allow using the repeat operator with a visual selection (!)
-- http://stackoverflow.com/a/8064607/127816
--vnoremap . :normal .<CR>

-- Easier movement between panes
vim.keymap.set("n", "<C-h>", "<C-w>h")
vim.keymap.set("n", "<C-j>", "<C-w>j")
vim.keymap.set("n", "<C-k>", "<C-w>k")
vim.keymap.set("n", "<C-l>", "<C-w>l")

-- Wrapped lines goes down/up to next row, rather than next line in file.
vim.keymap.set("", "j", "gj", { noremap = true, })
vim.keymap.set("", "k", "gk", { noremap = true, })

-- Clear search results
--nmap <leader>c :let @/=""<CR>
