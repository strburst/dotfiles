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
Plugin 'tpope/vim-repeat'
Plugin 'altercation/vim-colors-solarized'
Plugin 'mtth/scratch.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'bling/vim-airline'
Plugin 'sjl/gundo.vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'lervag/vimtex'
Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-unimpaired'
Plugin 'tpope/vim-fugitive'
" Plugin 'r0nk/slow-vim'
" Plugin 'jaxbot/semantic-highlight.vim'

call vundle#end()
filetype plugin indent on

"PLUGIN SETTINGS
let g:airline_powerline_fonts=1   "Use patched fonts properly
let g:gundo_help=0                "Hide help message in Gundo window

"Syntastic recommended settings
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_aggregate_errors = 1

let g:syntastic_mode_map = {
            \ "mode": "passive",
            \ "active_filetypes": [],
            \ "passive_filetypes": [] }

let g:syntastic_java_checkers = ["javac", "checkstyle"]
let g:syntastic_java_checkstyle_classpath="~/cs1332/checkstyle-6.2,jar"
let g:syntastic_java_checkstyle_conf_file="~/cs1332/CS1332-checkstyle.xml"

nnoremap <leader>s :SyntasticCheck<Cr>
nnoremap <leader>d :lnext<Cr>
nnoremap <leader>f :lprevious<Cr>
nnoremap <leader>g :lfirst<Cr>
nnoremap <leader>G :llast<Cr>

"Single line comments in java, c; stolen from jgkamat/dotfiles
let g:tcommentLineC = {
            \ 'commentstring': '// %s',
            \ 'replacements': g:tcomment#replacements_c
            \ }
call tcomment#DefineType('c', g:tcommentLineC)
call tcomment#DefineType('java', g:tcommentLineC)

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
set smartcase    "Case sensitive search if search has uppercase characters

set wildmode=longest,list:longest   "Autocomplete by longest match first

set splitbelow   "split below by default
set splitright   "vsplit right by default

set undofile           "Save undo tree between sessions
set undolevels=10000   "Keep more undo history (for Gundo playback timelapses)

set foldmethod=syntax   "Code folding by language
set foldlevel=1         "Start with one fold opened

"Automatically save and restore folds
autocmd BufWinLeave *.* mkview
autocmd BufWinEnter *.* silent loadview

set scrolloff=8   "Scroll up/down if cursor is 8 lines from the top/bottom

"When joining comments, remove the comment part
autocmd BufWinEnter,BufRead * setlocal formatoptions+=j
"Don't continue comments when creating a new line
autocmd BufWinEnter,BufRead * setlocal formatoptions-=o

"Usually overriden by editorconfig
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
inoremap jk <Esc>
inoremap kj <Esc>

"Bring up the undo tree
nnoremap <F4> :GundoToggle<CR>

"Turn off highlights for current search
nnoremap <F5> :nohl<CR>

"Open my vimrc in a new window/tab
nnoremap <F9> :vsplit $MYVIMRC<CR>
nnoremap <F10> :tabnew $MYVIMRC<CR>

"Toggle dark/light colorscheme
call togglebg#map("<F12>")

"Reload vimrc without restarting
nnoremap <leader>r :source $MYVIMRC<CR>:echo "vimrc reloaded"<CR>

"Open the current file with the default program
nnoremap <leader>o :!xdg-open <C-r>%<Cr>

"Enter Ex commands faster
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

"K joins previous line
nnoremap K kJ

"More consistent with C and D
nnoremap Y y$

"Exclude indentation when going to beginning of line by default
nnoremap ^ 0
nnoremap 0 ^

"Tab/shift-tab for autocompletion
inoremap <Tab> <C-P>
inoremap <S-Tab> <C-N>
inoremap <C-\> <Tab>

"Open/close folds
nnoremap <Space> za

"Since comma is the leader key
nnoremap \ ,

"Enter makes a line below the cursor in normal mode
nnoremap <Cr> o<Esc>
