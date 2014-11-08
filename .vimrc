"VUNDLE SETUP
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

"Vundle commands:
":PluginList, :PluginInstall, :PluginUpdate, :PluginSearch, :PluginClean

Plugin 'gmarik/Vundle.vim'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'tomtom/tcomment_vim'
Plugin 'tpope/vim-surround'
Plugin 'altercation/vim-colors-solarized'

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

set wildmode=longest,list:longest

set splitbelow "split below by default
set splitright "vsplit right by default

"VISUAL SETTINGS
syntax on
set t_Co=256
set background=dark
colorscheme solarized

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

"Turn off highlights for current search
nnoremap <F5> :nohl<CR>

"Open my vimrc in a new window/tab
nnoremap <F9> :vsplit $MYVIMRC<CR>
nnoremap <F10> :tabnew $MYVIMRC<CR>

"Toggle dark/light colorscheme
call togglebg#map("<F12>")

"Enter Ex commands faster
nnoremap ; :
nnoremap : ;

"Exclude indentation when going to beginning of line by default
nnoremap ^ 0
nnoremap 0 ^

"Window navigation without CTRL-W
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

"Tab/shift-tab for autocompletion
inoremap <Tab> <C-P>
inoremap <S-TAB> <C-N>
inoremap <C-\> <Tab>
