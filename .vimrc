"VUNDLE SETUP
set nocompatible
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

"Vundle commands:
":PluginList, :PluginInstall, :PluginUpdate, :PluginSearch, :PluginClean

Plugin 'gmarik/Vundle.vim'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'tomtom/tcomment_vim'

"Plugins to install/learn: surround, Ctrl-P, AG for Vim, fugitive, Command-T

call vundle#end()
filetype plugin indent on

"BASICS
set mouse=a        "Enable the mouse
set encoding=utf-8
set spelllang=en_us

"EDITOR BEHAVIOR
set errorbells   "Errors emit a bell character

set backupdir=~/.vim/backup "Backup file location
set directory=~/.vim/swap   "Swap file location
set undodir=~/.vim/undo     "Undo file location

set autoread   "Reread when files are changed outside of Vim

set autoindent
set expandtab    "Spaces for tabs
set shiftwidth=4
set softtabstop=4
set shiftround   "Round indents to multiples of shiftwidth
set backspace=indent,eol,start "Backspace over newlines

set ignorecase   "Searches ignore case by default
set smartcase    "Case sensetive search if search has uppercase characters

"VISUAL SETTINGS
syntax on
colorscheme ron

set number         "Show line numbers
set relativenumber "Show distance from the current line

set hlsearch     "Highlight search matches
set incsearch    "Begin showing search matches while typing

set colorcolumn=81
"set hl-ColorColumn=

set visualbell   "Visual, not auditory, alerts
set showcmd      "Shows partially completed key combinations
set title        "Allow vim to set the title of the console
set ruler        "Show line/col in the lower left
set laststatus=2 "Always show the status bar

"KEYMAPS
let mapleader=","
inoremap jk <ESC>
inoremap kj <ESC>

"Enter Ex commands faster
nnoremap ; :
nnoremap : ;

"Window navigation without CTRL-W
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

"Tab/shift-tab for autocompletion
inoremap <Tab> <C-P>
inoremap <S-TAB> <C-N>
