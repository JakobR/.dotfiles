set nocompatible
set nomodeline

runtime pathogen/autoload/pathogen.vim
execute pathogen#infect()

filetype on
filetype indent on
filetype plugin on

set background=dark
set nowrap
set scrolloff=2         " 2 lines above/below cursor when scrolling
set number              " show line numbers
set ruler               " show cursor position in status bar
set title               " show file in titlebar
set showmatch           " show matching brackets
set showmode            " show mode in status bar (insert/replace/...)
set showcmd             " show typed command in status bar
set wildmenu            " completion with menu
set laststatus=2        " use 2 lines for the status bar
set matchtime=1         " show matching bracket for 0.1 seconds
set matchpairs+=<:>     " for html

set esckeys             " map missed escape sequences (enables keypad keys)
set ignorecase          " case insensitive searching
set smartcase           " but become case sensitive if you type uppercase characters
set smartindent         " smart auto indenting
set smarttab            " smart tab handling for indenting
set magic               " change the way backslashes are used in search patterns
set bs=indent,eol,start " Allow backspacing over everything in insert mode

set tabstop=2           " number of spaces a tab counts for
set shiftwidth=2        " number of spaces for autoindents
set expandtab           " insert spaces instead of tabs

set mouse=a             " use mouse

" show tabs and trailing spaces (with low visibility)
set list listchars=tab:⇥\ ,trail:·
let g:solarized_visibility="low"

" color settings (if terminal/gui supports it)
if &t_Co > 2 || has("gui_running")
  syntax on          " enable colors
  set hlsearch       " highlight search (very useful!)
  set incsearch      " search incremently (search while typing)

  colorscheme solarized
endif

"set autochdir

" open NERDTree on the right side
let g:NERDTreeWinPos="right"
" open files/directories with single click
let g:NERDTreeMouseMode=3

let g:NERDTreeChDirMode=2

let g:CommandTMatchWindowAtTop=1
let g:CommandTAcceptSelectionMap = '<C-t>'
let g:CommandTAcceptSelectionTabMap = '<CR>'

" status line
" see http://got-ravings.blogspot.co.at/2008/08/vim-pr0n-making-statuslines-that-own.html
set statusline=%f       " file path relative to CWD
set statusline+=\ %m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%h      "help file flag
set statusline+=[%{strlen(&fenc)?&fenc:'none'}, "file encoding
set statusline+=%{&ff}] "file format
set statusline+=%y      "filetype
set statusline+=%{fugitive#statusline()} "git branch
set statusline+=%=      "left/right separator
set statusline+=%c,     "cursor column
set statusline+=%l/%L   "cursor line/total lines
set statusline+=\ %P    "percent through file
