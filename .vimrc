"" PLUGIN SETUP {{{

set nocompatible

if empty(glob('~/.vim/autoload/plug.vim'))
  " Automatically install vim-plug if not present
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

let mapleader=" "
let maplocalleader="\\"  " Actually just a single \

call plug#begin('~/.vim/plugged')

"" Core functionality {{{

" Vim plugin package manager
Plug 'gmarik/Vundle.vim'
" Configure indentation and other editor settings on a per-project basis
Plug 'editorconfig/editorconfig-vim'
" Syntax-checker support using the location list
Plug 'scrooloose/syntastic'
" Track editor statistics via wakatime
Plug 'wakatime/vim-wakatime'
" Awesome git integration
Plug 'tpope/vim-fugitive'
" Github integration for vim-fugitive
Plug 'tpope/vim-rhubarb'

"" }}} Keymaps and text objects {{{

" Comment/uncomment lines in nearly every language easily
Plug 'tpope/vim-commentary'
" Change delimiters like ( and [ easily
Plug 'tpope/vim-surround'
" Use the . command to repeat vim-surround mappings
Plug 'tpope/vim-repeat'
" Snippets/autocompletion plugin, default snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
" Mappings/text objects for C-style argument lists
Plug 'PeterRincker/vim-argumentative'
" Convenient paired mappings for [ and ] for navigation
Plug 'tpope/vim-unimpaired'
" Move selections around in visual block mode
Plug 'zirrostig/vim-schlepp'

"" }}} Navigation {{{

" Intelligent file opener
Plug 'ctrlpvim/ctrlp.vim'
" Navigate between vim/tmux splits easily
Plug 'christoomey/vim-tmux-navigator'

"" }}} Visual {{{

" Solarized colorscheme in vim
Plug 'altercation/vim-colors-solarized'
" Pretty statusline
Plug 'bling/vim-airline'
" Yet another color scheme
Plug 'baskerville/bubblegum'
" Solarized theme
Plug 'vim-airline/vim-airline-themes'
" Display open buffers in the statusline
Plug 'bling/vim-bufferline'
" Visualize the undo tree
Plug 'mbbill/undotree'

"" Language support {{{

" Better LaTeX support, e.g. comtinuous compilation mode
Plug 'lervag/vimtex'
" Mediawiki syntax support
Plug 'chikamichi/mediawiki.vim'
" Elixir language support
Plug 'elixir-lang/vim-elixir'
" Markdown language support
Plug 'tpope/vim-markdown'
" Markdown previewing
Plug 'suan/vim-instant-markdown'
" Better JSON language support
Plug 'elzr/vim-json'
" Alda language support
Plug 'daveyarwood/vim-alda'
" Rust language support
Plug 'rust-lang/rust.vim'

" }}} }}}

call plug#end()

"" }}} PLUGIN SETTINGS/MAPS {{{

let g:airline_powerline_fonts=1   " Use patched characters, not fallbacks

" Syntastic recommended settings
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=1
let g:syntastic_check_on_wq=0
let g:syntastic_aggregate_errors=1

" Only check a file if :SyntasticCheck used
let g:syntastic_mode_map = {
      \ "mode": "passive",
      \ "active_filetypes": [],
      \ "passive_filetypes": [] }

let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_javascript_eslint_exec = 'eslint_d'

nnoremap <leader>s :w<Cr>:SyntasticCheck<Cr>
nnoremap <leader>w :SyntasticReset<Cr>

let g:gundo_help=0   " Hide help message in Gundo window

" <C-p> expands snippets, rather than <Tab>
let g:UltiSnipsExpandTrigger="<C-p>"

" Bring up the undo tree
nnoremap <F4> :UndotreeToggle<Cr>

" EditorConfig doesn't manage git commit messages, etc
let g:EditorConfig_exclude_patterns = ['fugitive://.*']
" EditorConfig sets colorcolumn based on max_line_length
let g:EditorConfig_max_line_indicator = 'line'

" Enable syntax highlighting for some languages within Markdown
let g:markdown_fenced_languages = ['js=javascript']

" Don't preview Markdown when it's opened
let g:instant_markdown_autostart = 0

" CtrlP indexes hidden files; ignores VCS/dependency directories
let g:ctrlp_show_hidden = 1
let g:ctrlp_custom_ignore = {
      \ 'dir':  '\v[\/](\.(git|hg|svn)|node_modules)$' }

nnoremap <leader>m :CtrlPMRUFiles<Cr>

" Don't echo buffers automatically; airline already displays them
let g:bufferline_echo = 0

" Alias Gc to Gcommit
command! Gc Gcommit

" Schlep mappings
vmap <silent> <Up>    <Plug>SchleppUp
vmap <silent> <Down>  <Plug>SchleppDown
vmap <silent> <Left>  <Plug>SchleppLeft
vmap <silent> <Right> <Plug>SchleppRight

vmap Dk <Plug>SchleppDupUp
vmap Dj <Plug>SchleppDupDown
vmap Dh <Plug>SchleppDupLeft
vmap Dl <Plug>SchleppDupRight

"" }}} EDITOR BEHAVIOR {{{

set mouse=a        " Enable the mouse
set encoding=utf-8
set spelllang=en_us

set ttimeout
set ttimeoutlen=25

set backupdir=/tmp/vim/backup  " Backup file location
set directory=~/.vim/swap      " Swap file location
set undodir=~/.vim/undo        " Undo file location
set viewdir=/tmp/vim/view      " Code folding file location

" Create the proper directories if they don't exist
if !isdirectory("/tmp/vim/backup")
  call mkdir("/tmp/vim/backup", "p")
endif
if !isdirectory($HOME."/.vim/swap")
  call mkdir($HOME."/.vim/swap", "p")
endif
if !isdirectory($HOME."/.vim/undo")
  call mkdir($HOME."/.vim/undo", "p")
endif
if !isdirectory("/tmp/vim/view")
  call mkdir("/tmp/vim/view", "p")
endif

set dictionary+=/usr/share/dict/words

set autoread   " Reread when files are changed outside of Vim

set shiftround   " Round indents to multiples of shiftwidth
set backspace=indent,eol,start " Backspace over newlines

set ignorecase   " Searches ignore case by default
set smartcase    " Case sensitive search if search has uppercase characters

set showmatch   " Briefly jump back to the ( when a ) is typed

set wildmode=longest,list:longest   " Autocomplete by longest match first

set hidden   " Can switch out of buffers that have unsaved changes

set splitbelow   " split below by default
set splitright   " vsplit right by default

set undofile           " Save undo tree between sessions

set foldmethod=syntax   " Code folding by language
set foldlevel=1         " Start with one fold opened

" Automatically save and restore folds
autocmd BufWinLeave *.* mkview
autocmd BufWinEnter *.* silent loadview

set scrolloff=8   " Scroll up/down if cursor is 8 lines from the top/bottom

set tabpagemax=25 " Commands can auto-open up to 25 tabs

" When joining comments, remove the comment part
autocmd BufWinEnter,BufRead * setlocal formatoptions+=j

"  Usually set/overriden by editorconfig
set autoindent     " Copy indent from previous line when making a new line
set expandtab      " Use spaces instead of tabs
set shiftwidth=4   " Four spaces is one unit of indentation
set softtabstop=4  " Backspace at BOL in insert mode deletes four spaces

"" }}} VISUAL SETTINGS {{{

syntax on
set t_Co=256
set background=dark
colorscheme solarized

set number         " Show line numbers
set relativenumber " Show distance from the current line

set hlsearch     " Highlight search matches
set incsearch    " Begin showing search matches while typing
nohls            " Don't highlight everything again when sourcing vimrc

set cursorline       " Highlight the line the cursor is on

set visualbell t_vb=""   " Disable bells
set showcmd      " Shows partially completed key combinations
set title        " Allow vim to set the title of the console
set ruler        " Show line/col in the lower left
set laststatus=2 " Always show the status bar

set noshowmode   " Powerline plugin indicates modes already

"" }}} ASSORTED KEYMAPS {{{

" Turn off highlights for current search
nnoremap <leader>h :nohls<Cr>

" Edit/source my vimrc
nnoremap <F9> :edit $MYVIMRC<Cr>
nnoremap <F10> :source $MYVIMRC<Cr>:echo "vimrc reloaded"<Cr>

" Open the current file with the default program
nnoremap <leader>o :!xdg-open <C-r>%<Cr>

" Sort the current paragraph by line
nnoremap <leader>a !ipsort<Cr>
vnoremap <leader>a !sort<Cr>

" Insert the current ISO-formatted date on a new line
nnoremap <leader>; "=system("date --iso-8601")<Cr>p

" Swap : and ; to enter Ex commands faster
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

" K joins previous line
nnoremap K kJ

" Make Y more consistent with C and D
nnoremap Y y$

" Exclude indentation when going to beginning of line by default
noremap ^ 0
noremap 0 ^

" Tab/shift-tab for autocompletion
inoremap <Tab> <C-n>
inoremap <S-Tab> <C-p>
inoremap <C-\> <Tab>

" Open/close folds
nnoremap \\ za

" Repeat the last macro
nnoremap - @@

" Show man pages for commands
nnoremap <leader>k K

" Convert to unix line endings
nnoremap <leader>u :set ff=unix<Cr>:%s/<C-v><C-m>//g<Cr>

" Yank the whole buffer into the system clipboard
nnoremap <leader>y :%y +<Cr>

" Replace the entire file with the contents of the system clipboard
nnoremap <leader>t ggdG"+pkdd

" Switch to previously edited file
nnoremap <leader>f <C-^>

" Delete the buffer without closing the split
nnoremap <leader>d :bp\|bd #<Cr>

" Swap <C-r> and <C-r><C-p>; <C-r> inserts text literally instead of as typed
inoremap <C-r> <C-r><C-p>
inoremap <C-r><C-p> <C-r>

if filereadable(expand("~/.vimrc.local"))
    source ~/.vimrc.local
end

" }}}
