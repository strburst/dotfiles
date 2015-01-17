"VUNDLE SETUP
set nocompatible
filetype off

let mapleader=","
let maplocalleader="\\"

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

"Vundle commands:
":PluginList, :PluginInstall, :PluginUpdate, :PluginSearch, :PluginClean

Plugin 'gmarik/Vundle.vim'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'tomtom/tcomment_vim'
Plugin 'tpope/vim-surround'
Plugin 'altercation/vim-colors-solarized'
Plugin 'mtth/scratch.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'bling/vim-airline'
Plugin 'sjl/gundo.vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'lervag/vim-latex'
" Plugin 'jaxbot/semantic-highlight.vim'
"Plugins to install/learn: AG for Vim, fugitive, Command-T

call vundle#end()
filetype plugin indent on

"Plugin options
let g:airline_powerline_fonts=1   "Use patched fonts properly
let g:gundo_playback_delay=500    "Play back changes slowly

" BASICS
set mouse=a        "Enable the mouse
set encoding=utf-8
set spelllang=en_us

" EDITOR BEHAVIOR
set errorbells   "Errors emit a bell character
set ttimeoutlen=25

set backupdir=~/.vim/backup "Backup file location
set directory=~/.vim/swap   "Swap file location
set undodir=~/.vim/undo     "Undo file location
set viewdir=~/.vim/view     "Code folding file location

set autoread   "Reread when files are changed outside of Vim

set shiftround   "Round indents to multiples of shiftwidth
set backspace=indent,eol,start "Backspace over newlines

set ignorecase   "Searches ignore case by default
set smartcase    "Case sensetive search if search has uppercase characters

set wildmode=longest,list:longest

set splitbelow   "split below by default
set splitright   "vsplit right by default

set foldmethod=syntax   "Code folding by language
set foldlevel=1         "Start with one fold opened
"Automatically save and restore folds
autocmd BufWinLeave *.* mkview
autocmd BufWinEnter *.* silent loadview

set scrolloff=8   "Scroll up/down if cursor is 8 lines from the top/bottom

"These settings are usually overriden by editorconfig
set autoindent
set expandtab    "Spaces for tabs
set shiftwidth=4
set softtabstop=4

" VISUAL SETTINGS
syntax on
set t_Co=256
set background=dark
colorscheme solarized

set number         "Show line numbers
set relativenumber "Show distance from the current line

set hlsearch     "Highlight search matches
set incsearch    "Begin showing search matches while typing

set colorcolumn=81   "Vertical line at 81 characters
set cursorline       "Highlight the line the cursor is on

set visualbell   "Visual, not auditory, alerts
set showcmd      "Shows partially completed key combinations
set title        "Allow vim to set the title of the console
set ruler        "Show line/col in the lower left
set laststatus=2 "Always show the status bar

set noshowmode   "Powerline plugin indicates modes already

" KEYMAPS
inoremap jk <ESC>
inoremap kj <ESC>

"Bring up the undo tree
nnoremap <F4> :GundoToggle<CR>

"Turn off highlights for current search
nnoremap <F5> :nohl<CR>

"Reload my vimrc without restarting
nnoremap <F8> :source $MYVIMRC<CR>:echo "vimrc reloaded"<CR>

"Open my vimrc in a new window/tab
nnoremap <F9> :vsplit $MYVIMRC<CR>
nnoremap <F10> :tabnew $MYVIMRC<CR>

"Toggle dark/light colorscheme
call togglebg#map("<F12>")

"Enter Ex commands faster
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

"More consistent with C and D
nnoremap Y y$

"Exclude indentation when going to beginning of line by default
nnoremap ^ 0
nnoremap 0 ^

"Tab/shift-tab for autocompletion
inoremap <Tab> <C-P>
inoremap <S-TAB> <C-N>
inoremap <C-\> <Tab>

"Open/close folds
nnoremap <space> za
