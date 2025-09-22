vim.opt.compatible = false

-- Variables {{{

local HOME = os.getenv("HOME")

-- }}}

-- Plugins {{{

-- Clone 'mini.nvim' manually in a way that it gets managed by 'mini.deps'
local path_package = vim.fn.stdpath('data') .. '/site/'
local mini_path = path_package .. 'pack/deps/start/mini.nvim'
if not vim.loop.fs_stat(mini_path) then
  vim.cmd('echo "Installing `mini.nvim`" | redraw')
  local clone_cmd = {
    'git', 'clone', '--filter=blob:none',
    'https://github.com/echasnovski/mini.nvim', mini_path
  }
  vim.fn.system(clone_cmd)
  vim.cmd('packadd mini.nvim | helptags ALL')
  vim.cmd('echo "Installed `mini.nvim`" | redraw')
end

-- Set up 'mini.deps' (customize to your liking)
require('mini.deps').setup({ path = { package = path_package } })

-- Add all our plugin repos
MiniDeps.add("echasnovski/mini.nvim")
MiniDeps.add("miikanissi/modus-themes.nvim")
MiniDeps.add("neovim/nvim-lspconfig")
MiniDeps.add("stevearc/conform.nvim")
MiniDeps.add("tpope/vim-rsi")
MiniDeps.add("nvim-treesitter/nvim-treesitter")
MiniDeps.add("vrischmann/tree-sitter-templ")

-- Initialize all our UI-related plugins here, so we don't have the vim
-- equivalent of a "flash of unstyled content".
MiniDeps.now(function()
  require('mini.statusline').setup({
    use_icons = false,
  })
end)

MiniDeps.now(function()
  require('mini.basics').setup({
    options = {
      basic = true,
      extra_ui = true,
    },
    mappings = {
      basic = true,
      windows = true,
      move_with_alt = true,
    },
    autocommands = {
      basic = true,
    },
  })

  -- We try very hard to avoid using the signcolumn, but mini.basic enables it
  -- anyway so we just re-disable it.
  vim.opt.signcolumn = 'no'
end)
MiniDeps.later(function() require('mini.comment').setup() end)
MiniDeps.later(function() require('mini.completion').setup() end)
MiniDeps.later(function() require('mini.diff').setup() end)
MiniDeps.later(function() require('mini.git').setup() end)
MiniDeps.later(function()
  local hipatterns = require('mini.hipatterns')

  hipatterns.setup({
    highlighters = {
      -- Highlight hex color strings (`#rrggbb`) using that color
      hex_color = hipatterns.gen_highlighter.hex_color(),
    },
  })
end)
MiniDeps.later(function()
  require('mini.indentscope').setup({
    draw = {
      delay = 0,
      animation = require('mini.indentscope').gen_animation.none(),
    },
    symbol = '|',
  })
end)
MiniDeps.later(function()
  require('mini.misc').setup()
  MiniMisc.setup_auto_root()
end)
MiniDeps.later(function() require('mini.pairs').setup() end)
MiniDeps.later(function() require('mini.surround').setup() end)
MiniDeps.later(function()
  require('mini.pick').setup()
  vim.keymap.set('n', '<leader>ff', MiniPick.builtin.files, {})
  vim.keymap.set('n', '<leader>fg', MiniPick.builtin.grep_live, {})
  vim.keymap.set('n', '<leader>fb', MiniPick.builtin.buffers, {})
  vim.keymap.set('n', '<leader>fh', MiniPick.builtin.help, {})
end)

MiniDeps.later(function()
  require('conform').setup({
    formatters_by_ft = {
      go = { "goimports", "gofmt" },
      rust = { "rustfmt" },
      templ = { "templ" },
    },
    default_format_opts = {
      lsp_format = "prefer",
    },
    format_on_save = {
      -- These options will be passed to conform.format()
      timeout_ms = 500,
      lsp_format = "fallback",
    },
  })
end)

MiniDeps.later(function()
  vim.lsp.enable('gopls')
  vim.lsp.enable('rust_analyzer')
  vim.lsp.enable('templ')
  vim.lsp.config('html', {
    filetypes = { "html", "templ" },
  })
  vim.lsp.config('htmx', {
    filetypes = { "html", "templ" },
  })
end)

MiniDeps.later(function()
  require('nvim-treesitter.configs').setup({
    ensure_installed = { "templ" },
    highlight = {
      enable = true,
    }
  })
end)

-- }}}

-- Appearance {{{

MiniDeps.now(function()
  -- Colorscheme and syntax settings
  vim.opt.background = "dark"
  vim.cmd([[colorscheme modus]])
end)

-- Highlight VCS conflict markers
vim.cmd([[match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$']])

-- }}}

-- Line numbers
--
-- This includes some magic to disable relative numbers in insert mode and
-- disable absolute numbers in visual mode. It is roughly based on the
-- functionality of numbers.vim.
vim.opt.number = true
vim.opt.relativenumber = true

vim.api.nvim_create_autocmd('InsertEnter', {
  callback = function(event)
    vim.opt.number = true
    vim.opt.relativenumber = false
  end
})
vim.api.nvim_create_autocmd('InsertLeave', {
  callback = function(event)
    vim.opt.number = false
    vim.opt.relativenumber = true
  end
})

-- Various Settings {{{

-- While not strictly a keybind itself, setting the leader affects multiple
-- other keybinds so we make sure to set it.
vim.g.mapleader = ","

-- Default tab settings
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4

-- Random settings
vim.opt.hlsearch    = true      -- Hilight what we're searching for
vim.opt.showcmd     = true      -- Always show the currently entered command
vim.opt.ttyfast     = true      -- Make vim more responsive
vim.opt.lazyredraw  = true      -- Don't show intermediate macro steps
vim.opt.autowrite   = true      -- Write when switching buffers
vim.opt.colorcolumn = "+81"     -- PEP-8 usefulness
vim.opt.synmaxcol   = 800       -- Don't try to highlight lines longer than 800 chars
vim.opt.textwidth   = 80        -- Auto wrap comments at 80 chars
vim.opt.hidden      = true      -- Allow buffer switching without saving
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
