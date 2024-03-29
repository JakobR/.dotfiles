set nocompatible
filetype plugin indent on

runtime macros/matchit.vim
runtime! ftplugin/man.vim

if has('python')
    packadd! MatchTagAlways
endif

set background=dark
if $ZSH_THEME_BACKGROUND ==# 'light'
    set background=light
endif

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
set wildignore+=*.o
set wildignore+=*.pyc
set laststatus=2        " use 2 lines for the status bar
set matchtime=1         " show matching bracket for 0.1 seconds
set nrformats-=octal
set autoread            " Reload file when changed externally (but only if not yet changed in vim)
set ignorecase          " case insensitive searching
set smartcase           " but become case sensitive if you type uppercase characters
set smarttab            " smart tab handling for indenting
set magic               " change the way backslashes are used in search patterns
set bs=indent,eol,start " Allow backspacing over everything in insert mode
set tabstop=8           " width of actual <TAB> characters in the file
set softtabstop=4       " how many spaces to insert when pressing <TAB>
set shiftwidth=4        " number of spaces for autoindents
set expandtab           " insert spaces instead of tabs
set shiftround
set incsearch           " search incremently (search while typing)
set hlsearch            " highlight search (very useful!)
set display+=lastline
set display+=uhex
set history=1000
set tabpagemax=50
set mouse=a             " use mouse
set ttimeout
set ttimeoutlen=50
set updatetime=200
set nofoldenable        " folds should be open by default
set signcolumn=yes

" use system clipboard as default register
set clipboard=unnamed,unnamedplus

" Use <Space> as <Leader>
" See http://superuser.com/a/693644
map <Space> <Leader>

function s:AddToPath(dir)
  if isdirectory(a:dir)
    let &path .= ',' . escape(a:dir, ' ')
  endif
endfunction

call s:AddToPath('/usr/local/include')
call s:AddToPath('/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk/usr/include')
call s:AddToPath('/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1')
call s:AddToPath('/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/6.0/include')
call s:AddToPath('/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include')

" Save with C-s in terminal
" Note: this requires `stty -ixon`, see https://unix.stackexchange.com/a/59772
function Save()
    if &modified
        if empty(bufname('%'))
            " this doesn't seem to work, maybe a problem with some other part of my config
            browse confirm write
        else
            confirm write
        endif
    endif
endfunction
nnoremap <silent> <C-s> :call Save()<CR>
inoremap <silent> <C-s> <C-o>:call Save()<CR>
vnoremap <silent> <C-s> <Esc>:call Save()<CR>gv

" Set filetype for C++ headers without extension (like <iostream>)
" Standard library headers all seem to have a comment "// -*- C++ -*-" in the
" first line.
autocmd BufRead * if search('\V-*- C++ -*-', 'nwc', 1) | setlocal ft=cpp | endif

" Restore cursor position after reopening file
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" Use two spaces to separate sentences
" See http://stevelosh.com/blog/2012/10/why-i-two-space/
set cpo+=J

" Use <C-L> to clear the highlighting of :set hlsearch
function ClearOtherHighlighting()
    " nothing to do here currently
endfunction
" <C-L> is already used to redraw the screen, keep that functionality by
" executing the previous <C-L> at the end.
nnoremap <silent> <C-L> :nohlsearch<CR>:call ClearOtherHighlighting()<CR><C-L>

" show whitespace (with low visibility)
set list
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
if &termencoding ==# 'utf-8' || &encoding ==# 'utf-8'
  let &listchars = "tab:\u21e5 ,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u00b7"
  set showbreak=↪
endif
let g:solarized_visibility="low"

" if $COLORTERM ==# 'truecolor'
"     set termguicolors
" endif

" color settings (if terminal/gui supports it)
if &t_Co > 2 || has("gui_running")
  syntax on

  silent! colorscheme solarized8_high

  if &t_Co >= 256
    let g:indentLine_color_term = 235
  endif
endif


" ca w!! w !sudo tee > /dev/null %
ca w!! SudoWrite

" Change current directory to directory of the currently open file.
" Apparently setting 'autochdir' can cause problems with plugins, so I rather
" do it manually as needed instead of dealing hard-to-find bugs in plugins.
" See http://vim.wikia.com/wiki/Set_working_directory_to_the_current_file
nnoremap <leader>cd :cd %:p:h<CR>:pwd<CR>

" open URL under cursor
" see http://stackoverflow.com/questions/9458294/open-url-under-cursor-in-vim-with-browser
function OpenUrlUnderCursor()
    execute "normal! BvEy"
    " this regex supports one level of matching parentheses in URLs.
    " single closing parentheses are seen as end of the URL (because of link syntax in markdown)
    " (supporting matching parentheses actually breaks single opening parentheses as well)
    " A better way might be to enable this behaviour only in markdown files...
    let l:url=matchstr(@0, '\(http\|https\|mailto\)://\(([^ ]*)\|[^ >,;)]\)*')
    if l:url != ""
        if system('uname -s')=~'Darwin'
            silent exec "!open '".l:url."'" | redraw!
        else
            silent exec "!xdg-open '".l:url."'" | redraw!
        endif
        echo "opened ".l:url
    else
        echo "No URL under cursor."
    endif
endfunction
nmap <leader>o :call OpenUrlUnderCursor()<CR>

" Close all hidden buffers
" see https://stackoverflow.com/a/30101152
function! DeleteHiddenBuffers()
  let tpbl=[]
  let closed = 0
  call map(range(1, tabpagenr('$')), 'extend(tpbl, tabpagebuflist(v:val))')
  for buf in filter(range(1, bufnr('$')), 'bufexists(v:val) && index(tpbl, v:val)==-1')
    if getbufvar(buf, '&mod') == 0
      silent execute 'bwipeout' buf
      let closed += 1
    endif
  endfor
  echo "Closed ".closed." hidden buffers"
endfunction

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
    setlocal nowrap list
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

" Toggle wrapping with <Leader>w
noremap <silent> <Leader>w :call ToggleWrap()<CR>

" Enable wrapping in markdown and text files
autocmd FileType markdown,text silent call EnableWrap()

" Markdown settings, see: https://github.com/plasticboy/vim-markdown
let g:vim_markdown_math = 1
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_strikethrough = 1
let g:vim_markdown_new_list_item_indent = 2

" Don't show whitespace in man pages
autocmd FileType man setlocal nolist

let g:sql_type_default = 'pgsql'

" Configure browser for haskell_doc.vim
" http://projects.haskell.org/haskellmode-vim/
if system('uname -s')=~'Darwin'
  let g:haddock_browser = "open"
  let g:haddock_browser_callformat = "%s %s"
else
  " TODO: g:haddock_browser currently only set on OS X.
endif

autocmd FileType org setlocal textwidth=0

" Don't auto-insert comment-leader when hitting 'o' or 'O' in normal mode
" ftplugin is modifying this setting, see http://stackoverflow.com/a/16035812/1889401
autocmd FileType * setlocal formatoptions-=o

" Auto-insert leading '>' in literate haskell scripts
" http://stackoverflow.com/a/18572190
autocmd FileType lhaskell setlocal formatoptions+=ro

" Additional Haskell syntax highlighting
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

let g:lsc_server_commands = {
    \ 'haskell': 'hie --lsp'
    \ }

let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie', '--lsp']
    \ }


" Comment style by file type
autocmd FileType cmake setlocal commentstring=#\ %s
autocmd FileType qmake setlocal commentstring=#\ %s
autocmd FileType sparql setlocal commentstring=#\ %s
autocmd FileType matlab setlocal commentstring=%\ %s
autocmd FileType c,cpp,cs,java setlocal commentstring=//\ %s
autocmd FileType smt2 setlocal commentstring=;;\ %s
autocmd FileType tptp setlocal commentstring=%\ %s

let g:tex_flavor = "latex"   " Don't set the filetype to `plaintex` when creating new files
let g:tex_comment_nospell=1  " No spell check in LaTeX comments
let g:vimtex_view_method = 'skim'
" let g:vimtex_fold_enabled=0
let g:vimtex_fold_manual=1
" disable extended highlighting of matching delimiters (it seems to be messing up the cursor)
let g:vimtex_matchparen_enabled = 0

" Apparently the combination of vimtex and relativenumber really slows vim down
autocmd FileType tex call NoRelativeNumbers()
function NoRelativeNumbers()
    NumbersDisable
    setlocal norelativenumber
    setlocal number
endfunction

" LaTeX compilation
autocmd FileType tex let &l:makeprg="latexmk -outdir=".shellescape(expand('%:p:h'),1)." -pdflatex='pdflatex -file-line-error -synctex=1 -interaction=nonstopmode -halt-on-error' -pdf ".shellescape(expand('%:p'),1)
" autocmd BufWritePost *.tex silent Make

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
let g:ctrlp_tabpage_position = 'al'
let g:ctrlp_show_hidden = 1
" Ignore the following directories:
"   .git
"   .hg
"   .svn
"   .dotfiles/vim/bundle
"   .dotfiles/zsh/oh-my-zsh
" TODO: check regex for build/bin directory... it should not just compare the suffix!!
" And files with one of the following extensions:
"   exe, so, dll, jar, o, hi, dyn_hi, dyn_o
let g:ctrlp_custom_ignore = {
  \ 'dir': '\v(\/\.(git|hg|svn|stack-work|cargo-target)|\.dotfiles\/(vim\/bundle|zsh\/oh-my-zsh)|build(-.*)?|bin|obj)$',
  \ 'file': '\v(\.(exe|so|dll|jar|o|hi|dyn_hi|dyn_o)|\/vampire_[^.]+)$'
  \ }

nmap <F4> <Plug>(altr-forward)
nmap <S-F4> <Plug>(altr-back)

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
set statusline+=[%{strlen(&fenc)?&fenc:'none'}  " file encoding
set statusline+=%{&ff=='unix'?'':','.&ff}       " file format, unless it's unix
if exists('+bomb')
  set statusline+=%{&bomb?',BOM':''}            " BOM if byte order mark is present
end
set statusline+=]
set statusline+=\ %y    " filetype
set statusline+=%=      " left/right separator
" set statusline+=%{rvm#statusline()}
set statusline+=\ [%b][0x%B]\                   " ASCII and byte code under cursor
set statusline+=%c,     " cursor column
set statusline+=%l/%L   " cursor line/total lines
set statusline+=\ %P    " percent through file

" Make Y consistent with C and D. See :help Y.
nnoremap Y y$

" gf should open files in a new tab
nnoremap gf <C-W>gf

" disable ex mode
nnoremap Q <nop>

" reselect visual block after indent
vnoremap < <gv
vnoremap > >gv

vmap v <Plug>(expand_region_expand)

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
  " TODO: Check if permissions of s:dir are 700 (with python? or shell commands?)

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

" include a timestamp in backup extension
" TODO: Auto-cleanup, or at least show a message if the directory is getting too full
autocmd BufWritePre * let &backupext = '-' . strftime("%Y%b%d-%X") . '~'
set backup

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
" let g:gitgutter_max_signs = 500  " default value is 500
" also allow ]h and [h for navigation
nmap ]h <Plug>(GitGutterNextHunk)
nmap [h <Plug>(GitGutterPrevHunk)
" hunk-add and hunk-revert
nmap <Leader>ha <Plug>(GitGutterStageHunk)
nmap <Leader>hr <Plug>(GitGutterUndoHunk)

" Use <Leader>s to search in project
nnoremap <Leader>s :Ack<Space>
if executable('ag')
  let g:ackprg = 'ag --vimgrep --smart-case'
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

" Disable messages for completion menu, see https://groups.google.com/forum/#!topic/vim_dev/WeBBjkXE8H8
if v:version > 704 || (v:version == 704 && has('patch314'))
    set shortmess+=c
endif


" " make YCM compatible with UltiSnips (using supertab)
" let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
" let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
" let g:SuperTabDefaultCompletionType = '<C-n>'
" " better key bindings for UltiSnipsExpandTrigger
" let g:UltiSnipsExpandTrigger = "<tab>"
" let g:UltiSnipsJumpForwardTrigger = "<tab>"
" let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
let g:UltiSnipsExpandTrigger = "<C-j>"
let g:UltiSnipsJumpForwardTrigger = "<C-j>"
let g:UltiSnipsJumpBackwardTrigger = "<C-k>"

set completeopt-=preview
let g:ycm_add_preview_to_completeopt = 0
autocmd FileType c,cpp setlocal completeopt+=preview

let g:ycm_allow_changing_updatetime = 0

let g:ycm_enable_diagnostic_signs = 0

" Load .ycm_extra_conf.py only in my own projects
let g:ycm_extra_conf_globlist = ['!~/code/other/*','~/code/*','~/Documents/Uni/*','!~/*']
let g:ycm_global_ycm_extra_conf = $JR_DOTFILES . '/ycm_extra_conf.py'
" Key to invoke semantic completion. Useful for searching top-level functions
" and classes. (By default, semantic completion is only triggered
" automatically after typing `.`, `->`, and `::`).
" See https://github.com/Valloric/YouCompleteMe#the-gycm_key_invoke_completion-option
let g:ycm_key_invoke_completion = '<C-l>'

" let b:delimitMate_expand_cr = 1

let g:syntastic_always_populate_loc_list = 1
let g:ycm_always_populate_location_list = 1

let g:ycm_language_server =
\ [
\   {
\     'name': 'rust',
\     'cmdline': ['rust-analyzer'],
\     'filetypes': ['rust'],
\     'project_root_files': ['Cargo.toml']
\   }
\ ]

let g:syntastic_aggregate_errors = 1

let g:syntastic_mode_map = {
    \ "mode": "active",
    \ "active_filetypes": [],
    \ "passive_filetypes": ["java", "tex", "haskell", "rust"] }
function RunSyntasticAndJumpToError()
    if &ft == 'haskell'
        echoerr 'TODO: implement RunSyntasticAndJumpToError for ft=haskell'
    else
        " Run Syntastic for everything else
        if exists('b:syntastic_mode')
            let l:old_syntastic_mode = b:syntastic_mode
        endif
        " Don't check twice if this is an active filetype
        let b:syntastic_mode = 'passive'
        write
        SyntasticCheck
        if exists('l:old_syntastic_mode')
            let b:syntastic_mode = l:old_syntastic_mode
        else
            unlet b:syntastic_mode
        endif
        Errors
        lclose
        " If there are, in fact, errors, the ll command will jump to the first one
        " and thus replace this echoed message with the message from syntastic
        echo 'No errors'
        silent! ll 1
    endif
endfunction
nnoremap <Leader>e :call RunSyntasticAndJumpToError()<CR>

" Don't need pylint in addition to flake8
let g:syntastic_python_checkers = ['python', 'flake8']  ", 'mypy']
" Ignore errors that are more annoying than helpful
" E501 is "line too long"
let g:syntastic_python_flake8_post_args = '--ignore=E501,W503'
" Use python3 by default
let g:syntastic_python_python_exec = 'python3'
let g:syntastic_python_flake8_exe = 'python3 -mflake8'
" python3 for semantic completion
let g:ycm_python_binary_path = '/usr/local/bin/python3'

" Allow switching to python 2 for specific buffers
function UsePython2()
  let b:syntastic_python_python_exec = 'python2'
  let b:syntastic_python_flake8_exe = 'python2 -mflake8'
  " let b:ycm_python_binary_path = '/usr/local/bin/python2'  " not sure whether this works...
endfunction

let python_highlight_all = 1

" no need to have both lacheck and chktex
let g:syntastic_tex_checkers = ['chktex']

let g:syntastic_haskell_checkers = ['hlint']
let g:ycm_semantic_triggers = {'haskell' : ['.']}
let g:necoghc_enable_detailed_browse = 1
autocmd FileType haskell,lhaskell setlocal omnifunc=necoghc#omnifunc

function ShowType()
    if index(['c', 'cpp'], &filetype) != -1
        " C and C++ type information via YouCompleteMe
        if exists(':YcmCompleter')
            YcmCompleter GetType
        else
            echoerr ":YcmCompleter is not available."
        endif
    elseif &ft == 'haskell'
        if exists('g:did_coc_loaded')
            call CocAction('doHover')
        else
            echoerr "Coc not loaded"
        endif
    else
        echoerr "No type information for filetype " . &filetype
    endif
endfunction
nnoremap <Leader>t :call ShowType()<CR>

" TODO: If GoTo doesn't work, that means YCM is currently compiling your file. Consider using GoToImprecise to not re-trigger compilation.
autocmd FileType c,cpp,rust nnoremap <buffer> <C-]> :YcmCompleter GoTo<CR>


augroup Templating
    autocmd!
    autocmd BufNewFile * silent! call LoadMkTemplate()
augroup END

function LoadMkTemplate()
    0 read !mk --write-to-stdout -p vim "%"
    " Loading a template into an empty buffer leaves an extra blank line at the bottom, delete it
    execute line('$') . "d _"
    " The cursor command will now be on the last line
    let l:cursorcmd = getline('$')
    execute line('$') . "d _"
    execute l:cursorcmd
endfunction


" secure modelines
let g:secure_modelines_modelines=10

" Disable ansiesc in vimpager (I prefer the highlighting from tpope/vim-git
" for git output, which is where I use vimpager most of the time)
let vimpager_disable_ansiesc = 1

" Project-specific settings
" This should really be in a project-local file or something, but 'set exrc'
" has serious security issues and I didn't want to choose a plugin at the moment.
" This one seems to be actively developed, though: https://github.com/LucHermitte/local_vimrc
function IsProject(path, name)
  " Name should be the partial path of the project relative to home directory
  " e.g for project at '$HOME/code/blah' use 'code/blah' as name argument.
  return a:path =~ '\V\^' . $HOME . '/' . a:name
endfunction
function ProjectSpecificSetup()
  let l:path = expand('%:p')
  if IsProject(l:path, 'code/anki-addons')
    call UsePython2()   " Anki uses python 2.7.6
  elseif IsProject(l:path, 'code/vampire')
      setlocal ts=8
      setlocal sts=2
      setlocal sw=2
  " elseif IsProject(l:path, 'code/something_else')
  "   " something else
  " elseif l:path =~ '\V\^/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1'
  "   setlocal ft=cpp
  elseif l:path =~ '\V\.\*libxml2-2.\..\.\+'    " libxml2-2.?.? (mixed tabs and spaces for indentation)
    set ts=8
  endif
endfunction
autocmd BufReadPost,BufNewFile * call ProjectSpecificSetup()

" When calling vim from a terminal via the $EDITOR variable, switch back to
" the terminal after the edit is done
:command ActivateTerminalOnExit autocmd VimLeave * !/usr/bin/open -a iTerm

" Settings with security issues (this part should be at the end of vimrc)
"
" Disable modeline
set nomodeline
" " Enable project-specific configuration with secure mode
" set exrc
" " NOTE:
" " This prevents certain commands, but only if the file is not owned by yourself,
" " which means it doesn't offer any protection if you just clone random github
" " repositories or open suspicious tarballs.
" set secure
