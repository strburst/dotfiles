" VUNDLE SETUP
set nocompatible
filetype off

let mapleader=","
let maplocalleader="\\"

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Vim plugin package manager
Plugin 'gmarik/Vundle.vim'
" Configure indentation, other editor settings on a per-project basis
Plugin 'editorconfig/editorconfig-vim'
" Comment/uncomment lines in nearly every language easily
Plugin 'tomtom/tcomment_vim'
" Change delimiters like ( and [ easily
Plugin 'tpope/vim-surround'
" Use the . command to repeat vim-surround mappings
Plugin 'tpope/vim-repeat'
" Solarized colorscheme in vim
Plugin 'altercation/vim-colors-solarized'
" Scratch area
Plugin 'mtth/scratch.vim'
" Intelligent file opener
Plugin 'kien/ctrlp.vim'
" Pretty statusline display
Plugin 'bling/vim-airline'
" Visualize the undo tree
Plugin 'sjl/gundo.vim'
" Navigate between vim/tmux splits easily
Plugin 'christoomey/vim-tmux-navigator'
" Better LaTeX support, e.g. comtinuous compilation mode
Plugin 'lervag/vimtex'
" Syntax-checker support using the location list
Plugin 'scrooloose/syntastic'
" Convenient paired mappings for [ and ] for navigation
Plugin 'tpope/vim-unimpaired'
" Awesome git integration
Plugin 'tpope/vim-fugitive'
" Mediawiki syntax support
Plugin 'chikamichi/mediawiki.vim'
" Track editor statistics via wakatime
Plugin 'wakatime/vim-wakatime'
" Elixir language support
Plugin 'elixir-lang/vim-elixir'
" Convenient notes plugin, dependency
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-notes'
" Snippets/autocompletion plugin, default snippets
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'

call vundle#end()
filetype plugin indent on

" PLUGIN SETTINGS/MAPS
" Syntastic recommended settings
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_aggregate_errors = 1

" Only check a file if :SyntasticCheck used
let g:syntastic_mode_map = {
            \ "mode": "passive",
            \ "active_filetypes": [],
            \ "passive_filetypes": [] }

nnoremap <leader>s :w<Cr>:SyntasticCheck<Cr>

" Single line comments in java, c; stolen from jgkamat/dotfiles
let g:tcommentLineSlashes = {
            \ 'commentstring': '// %s',
            \ 'replacements': g:tcomment#replacements_c
            \ }
call tcomment#DefineType('c', g:tcommentLineSlashes)
call tcomment#DefineType('java', g:tcommentLineSlashes)

let g:gundo_help=0   " Hide help message in Gundo window

" <C-p> expands snippets, rather than <Tab>
let g:UltiSnipsExpandTrigger="<C-p>"

" Bring up the undo tree
nnoremap <F4> :GundoToggle<Cr>

" Directory to store vim-notes
let g:notes_directories = ['~/.vim/notes']

" EDITOR BEHAVIOR
set mouse=a        " Enable the mouse
set encoding=utf-8
set spelllang=en_us

set ttimeoutlen=25

set backupdir=~/.vim/backup " Backup file location
set directory=~/.vim/swap   " Swap file location
set undodir=~/.vim/undo     " Undo file location
set viewdir=~/.vim/view     " Code folding file location

" set path+=system("echo $PATH | sed 's/:/,/g'")

set autoread   " Reread when files are changed outside of Vim

set shiftround   " Round indents to multiples of shiftwidth
set backspace=indent,eol,start " Backspace over newlines

set ignorecase   " Searches ignore case by default
set smartcase    " Case sensitive search if search has uppercase characters

set wildmode=longest,list:longest   " Autocomplete by longest match first

set splitbelow   " split below by default
set splitright   " vsplit right by default

set undofile           " Save undo tree between sessions
set undolevels=10000   " Keep more undo history (for Gundo playback timelapses)

set foldmethod=syntax   " Code folding by language
set foldlevel=1         " Start with one fold opened

" Automatically save and restore folds
autocmd BufWinLeave *.* mkview
autocmd BufWinEnter *.* silent loadview

set scrolloff=8   " Scroll up/down if cursor is 8 lines from the top/bottom

set tabpagemax=25 " Commands can auto-open up to 25 tabs

" When joining comments, remove the comment part
autocmd BufWinEnter,BufRead * setlocal formatoptions+=j
" Don't continue comments when creating a new line
autocmd BufWinEnter,BufRead * setlocal formatoptions-=o

"  Usually set/overriden by editorconfig
set autoindent
set expandtab    " Spaces for tabs
set shiftwidth=4
set softtabstop=4

"  VISUAL SETTINGS
syntax on
set t_Co=256
set background=dark
colorscheme solarized

set number         " Show line numbers
set relativenumber " Show distance from the current line

set hlsearch     " Highlight search matches
set incsearch    " Begin showing search matches while typing

set colorcolumn=81   " Vertical line at 81 characters
set cursorline       " Highlight the line the cursor is on

set visualbell t_vb=""   " Disable bells
set showcmd      " Shows partially completed key combinations
set title        " Allow vim to set the title of the console
set ruler        " Show line/col in the lower left
set laststatus=2 " Always show the status bar

set noshowmode   " Powerline plugin indicates modes already

" ASSORTED KEYMAPS
" Turn off highlights for current search
nnoremap <F5> :nohl<Cr>

" Open my vimrc in a new window/tab
nnoremap <F9> :vsplit $MYVIMRC<Cr>
nnoremap <F10> :tabnew $MYVIMRC<Cr>

" Toggle dark/light colorscheme
call togglebg#map("<F12>")

" Reload vimrc without restarting
nnoremap <leader>r :source $MYVIMRC<Cr>:echo "vimrc reloaded"<Cr>

" Open the current file with the default program
nnoremap <leader>o :!xdg-open <C-r>%<Cr>

" Sort the current paragraph by line
nnoremap <leader>a !ipsort<Cr>
vnoremap <leader>a !sort<Cr>

" Insert the current, ISO-formatted date
nnoremap <leader>; a<C-r>=system("date --iso-8601")<Cr><Bs><Esc>
inoremap <leader>; <C-r>=system("date --iso-8601")<Cr><Bs>

" Alternative to esc
inoremap jk <Esc>
inoremap kj <Esc>

" Enter Ex commands faster
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

" K joins previous line
nnoremap K kJ

" More consistent with C and D
nnoremap Y y$

" Exclude indentation when going to beginning of line by default
noremap ^ 0
noremap 0 ^

" Tab/shift-tab for autocompletion
inoremap <Tab> <C-P>
inoremap <S-Tab> <C-N>
inoremap <C-\> <Tab>

" Open/close folds
nnoremap <Space> za

" Since comma is the leader key
nnoremap \ ,

" Repeat the last macro
nnoremap - @@

" Switch between/move tabs
nnoremap _ gT
nnoremap + gt
nnoremap <C-_> :-tabmove<Cr>
nnoremap <C-+> :+tabmove<Cr>
