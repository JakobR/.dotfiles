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

" color settings (if terminal/gui supports it)
if &t_Co > 2 || has("gui_running")
  syntax on          " enable colors
  set hlsearch       " highlight search (very useful!)
  set incsearch      " search incremently (search while typing)

  " TODO: Fix terminal colors before using that
  "colorscheme solarized
endif
