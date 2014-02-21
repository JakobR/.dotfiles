" Load bundles
if empty($JR_DOTFILES)
  source $HOME/.dotfiles/bundles.vim
else
  source $JR_DOTFILES/bundles.vim
endif

filetype plugin indent on

runtime macros/matchit.vim

set background=dark
set nowrap
set scrolloff=2         " 2 lines above/below cursor when scrolling
set sidescrolloff=5
set number              " show line numbers
set ruler               " show cursor position in status bar
set title               " show file in titlebar
set showmatch           " show matching brackets
set showmode            " show mode in status bar (insert/replace/...)
set showcmd             " show typed command in status bar
set wildmenu            " completion with menu
set wildmode=list:longest,full
set laststatus=2        " use 2 lines for the status bar
set matchtime=1         " show matching bracket for 0.1 seconds
set nrformats-=octal
set autoread            " Reload file when changed externally (but only if not yet changed in vim)
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
set shiftround
set autoindent
set incsearch           " search incremently (search while typing)
set hlsearch            " highlight search (very useful!)
set display+=lastline
set display+=uhex
set history=1000
set tabpagemax=50
set mouse=a             " use mouse
set ttimeout
set ttimeoutlen=50
set updatetime=750

set path+=/usr/local/include

" Use two spaces to separate sentences
" See http://stevelosh.com/blog/2012/10/why-i-two-space/
set cpo+=J

" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
  nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
endif

" show whitespace (with low visibility)
set list
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
if &termencoding ==# 'utf-8' || &encoding ==# 'utf-8'
  let &listchars = "tab:\u21e5 ,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u00b7"
endif
let g:solarized_visibility="low"

" color settings (if terminal/gui supports it)
if &t_Co > 2 || has("gui_running")
  syntax on
  silent! colorscheme solarized

  if &t_Co >= 256
    let g:indentLine_color_term = 235
  endif
endif

" Change current directory to directory of the currently open file.
" Apparently setting 'autochdir' can cause problems with plugins, so I rather
" do it manually as needed instead of dealing hard-to-find bugs in plugins.
" See http://vim.wikia.com/wiki/Set_working_directory_to_the_current_file
nnoremap <leader>cd :cd %:p:h<CR>:pwd<CR>

" open URL under cursor
" see http://stackoverflow.com/questions/9458294/open-url-under-cursor-in-vim-with-browser
function OpenUrlUnderCursor()
  if system('uname')=~'Darwin'
    execute "normal! BvEy"
    " this regex supports one level of matching parentheses in URLs.
    " single closing parentheses are seen as end of the URL (because of link syntax in markdown)
    " (supporting matching parentheses actually breaks single opening parentheses as well)
    " A better way might be to enable this behaviour only in markdown files...
    let url=matchstr(@0, '\(http\|https\|mailto\)://\(([^ ]*)\|[^ >,;)]\)*')
    if url != ""
      silent exec "!open '".url."'" | redraw!
      echo "opened ".url
    else
      echo "No URL under cursor."
    endif
  else
    echo "TODO: OpenUrlUnderCursor() currently only works on OS X."
  endif
endfunction
nmap <leader>o :call OpenUrlUnderCursor()<CR>

" Functions to enable/disable wrapping
function EnableWrap()
  if !&wrap
    echo "Wrap ON"
    setlocal wrap linebreak nolist
    "set virtualedit=
    "setlocal display+=lastline
    noremap  <buffer> <silent> <Up>   gk
    noremap  <buffer> <silent> <Down> gj
    noremap  <buffer> <silent> <Home> g<Home>
    noremap  <buffer> <silent> <End>  g<End>
    inoremap <buffer> <silent> <Up>   <C-o>gk
    inoremap <buffer> <silent> <Down> <C-o>gj
    inoremap <buffer> <silent> <Home> <C-o>g<Home>
    inoremap <buffer> <silent> <End>  <C-o>g<End>
  endif
endfunction

function DisableWrap()
  if &wrap
    echo "Wrap OFF"
    setlocal nowrap
    "set virtualedit=all
    silent! nunmap <buffer> <Up>
    silent! nunmap <buffer> <Down>
    silent! nunmap <buffer> <Home>
    silent! nunmap <buffer> <End>
    silent! iunmap <buffer> <Up>
    silent! iunmap <buffer> <Down>
    silent! iunmap <buffer> <Home>
    silent! iunmap <buffer> <End>
  end
endfunction

function ToggleWrap()
  if &wrap
    call DisableWrap()
  else
    call EnableWrap()
  endif
endfunction

" Use <Space> as <Leader>
" See http://superuser.com/a/693644
map <Space> <Leader>

" Toggle wrapping with <Leader>w
noremap <silent> <Leader>w :call ToggleWrap()<CR>

" Enable wrapping in markdown and text files
autocmd FileType markdown,text silent call EnableWrap()

" Don't show whitespace in man pages
autocmd FileType man set nolist

let g:sql_type_default = 'pgsql'

" Configure browser for haskell_doc.vim
" http://projects.haskell.org/haskellmode-vim/
if system('uname')=~'Darwin'
  let g:haddock_browser = "open"
  let g:haddock_browser_callformat = "%s %s"
else
  " TODO: g:haddock_browser currently only set on OS X.
endif

" Don't auto-insert comment-leader when hitting 'o' or 'O' in normal mode
" ftplugin is modifying this setting, see http://stackoverflow.com/a/16035812/1889401
autocmd FileType * setlocal formatoptions-=o

" Auto-insert leading '>' in literate haskell scripts
" http://stackoverflow.com/a/18572190
autocmd FileType lhaskell setlocal formatoptions+=ro

" HTML indentation
let g:html_indent_inctags = "body,head,li"
let g:html_indent_script1 = "inc"
let g:html_indent_style1 = "inc"

" open NERDTree on the right side
let g:NERDTreeWinPos="right"
" open files/directories with single click
"let g:NERDTreeMouseMode=3

let g:NERDTreeChDirMode=2

map  <leader>' :NERDTreeTabsToggle<CR>
imap <leader>' <C-O>:NERDTreeTabsToggle<CR>

let g:ctrlp_match_window_bottom = 0
let g:ctrlp_match_window_reversed = 0
let g:ctrlp_max_height = 15
let g:ctrlp_prompt_mappings = {
    \ 'AcceptSelection("e")': ['<c-t>'],
    \ 'AcceptSelection("t")': ['<cr>', '<2-LeftMouse>'],
    \ }

nnoremap <F5> :UndotreeToggle<CR>
inoremap <F5> <C-O>:UndotreeToggle<CR>

" status line
" see http://got-ravings.blogspot.co.at/2008/08/vim-pr0n-making-statuslines-that-own.html
" and http://stackoverflow.com/questions/5375240/a-more-useful-statusline-in-vim
set statusline=%f       " file path relative to CWD
set statusline+=%m      " modified flag
set statusline+=%r      " read only flag
set statusline+=%h      " [Help] file flag
set statusline+=%w\     " [Preview] file flag
set statusline+=%{exists('g:loaded_fugitive')?fugitive#statusline():''}\      " git branch
set statusline+=[%{strlen(&fenc)?&fenc:'none'} " file encoding
set statusline+=%{&ff=='unix'?'':','.&ff}]     " file format, unless it's unix (TODO: Print that in red)
set statusline+=%=      " left/right separator
" set statusline+=%{rvm#statusline()}
set statusline+=%y\     " filetype
set statusline+=%c,     " cursor column
set statusline+=%l/%L   " cursor line/total lines
set statusline+=\ %P    " percent through file

" Make Y consistent with C and D. See :help Y.
nnoremap Y y$

" gf should open files in a new tab
nnoremap gf <C-W>gf

" reselect visual block after indent
vnoremap < <gv
vnoremap > >gv

function EnsureDirectoryExists(dir)
  if empty(glob(a:dir))
    if exists('*mkdir')
      call mkdir(a:dir, '', 0700)
    else
      echoerr 'mkdir is not available. Please create this directory with permissions 700: ' . a:dir
    endif
  endif
  if isdirectory(a:dir)
    return 1
  else
    echoerr 'Not a directory: ' . a:dir
    return 0
  endif
endfunction

let s:dir = expand('~/.vim-tmp')
if EnsureDirectoryExists(s:dir)
  " TODO: Check if permissions of s:dir are 700

  let s:swapdir = s:dir . '/swap'
  if EnsureDirectoryExists(s:swapdir)
    " need double slashes at end to build swap file names from full path
    let &directory = s:swapdir . '//,' . &directory
  endif

  let s:backupdir = s:dir . '/backup'
  if EnsureDirectoryExists(s:backupdir)
    let &backupdir = s:backupdir . '//,' . &backupdir
  endif

  let s:undodir = s:dir . '/undo'
  if exists('+undodir') && EnsureDirectoryExists(s:undodir)
    let &undodir = s:undodir . '//,' . &undodir
  endif
endif
if exists('+undofile')
  set undofile
endif

" Cursor shape in iTerm2
if !has("gui_running") && ($TERM_PROGRAM ==# 'iTerm.app')
  " from https://gist.github.com/andyfowler/1195581
  "   tmux will only forward escape sequences to the terminal if surrounded by a DCS sequence
  "   http://sourceforge.net/mailarchive/forum.php?thread_name=AANLkTinkbdoZ8eNR1X2UobLTeww1jFrvfJxTMfKSq-L%2B%40mail.gmail.com&forum_name=tmux-users
  if exists('$TMUX')
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
  else
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
  endif
endif

" tab switching is too slow otherwise
let g:gitgutter_eager = 0

" Use <Leader>a to search in project
if executable('ag')
  " ag aka the_silver_searcher
  " see https://github.com/ggreer/the_silver_searcher
  nnoremap <Leader>a :Ag<Space>
elseif executable('ack')
  nnoremap <Leader>a :Ack<Space>
else
  nnoremap <Leader>a :echoerr 'Error: Neither ag nor ack are available!'<CR>
endif

" Highlight matching tags in these file types (MatchTagAlways plugin)
let g:mta_filetypes = {
    \ 'html' : 1,
    \ 'xhtml' : 1,
    \ 'xml' : 1,
    \ 'jinja' : 1,
    \ 'eruby' : 1,
    \ 'php' : 1,
    \ 'ant' : 1
    \}

" Use MatchTag highlighting group for matching tags (and not MatchParen)
let g:mta_use_matchparen_group = 0

" Disable messages for completion menu
" see patch: https://groups.google.com/forum/#!topic/vim_dev/WeBBjkXE8H8
" TODO: Activate as soon as this patch is pulled into main vim
" if v:version > 704 || (v:version == 704 && has('patch???'))
"   set shortmess+=c
" endif

set completeopt-=preview
let g:ycm_add_preview_to_completeopt = 0

let g:ycm_allow_changing_updatetime = 0

" Load .ycm_extra_conf.py only in my own projects
let g:ycm_extra_conf_globlist = ['!~/code/other/*','~/code/*','!~/*']

" Make Eclim play nicely with YouCompleteMe
let g:EclimCompletionMethod = 'omnifunc'

" Disable Eclim for now
let g:EclimDisabled = 1

" let b:delimitMate_expand_cr = 1

" TODO:
" Configure vim for Objective-C and Cocoa development
" http://stackoverflow.com/a/11550394
" https://github.com/msanders/cocoa.vim
" https://github.com/b4winckler/vim-objc
" https://github.com/eraserhd/objective-vim/
" http://objvimmer.com/blog/2012/12/05/getting-started-with-ios-development-with-vim/

" secure modelines
let g:secure_modelines_modelines=10
" disable secure modelines and indent guides in vimpager
if exists('g:vimpager')
  let g:loaded_securemodelines = 1
  let g:indentLine_loaded = 1
endif

" Disable modeline (security issue)
" This should be the last line
set nomodeline
