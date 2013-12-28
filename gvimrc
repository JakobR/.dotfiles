set lines=50
set columns=120

set guifont=Menlo:h14

set background=light
colorscheme solarized

" from https://github.com/raecoo/dotfiles/blob/master/gvimrc
if has("gui_macvim")
  " Cmd-\ toggles NERDTree
  noremap <D-\> :NERDTreeTabsToggle<CR>
  inoremap <D-\> <C-O>:NERDTreeTabsToggle<CR>
  " Cmd-\ is frequently broken, but I am unable to reproduce it consistently
  " Use Cmd=' as alternative
  noremap <D-'> :NERDTreeTabsToggle<CR>
  inoremap <D-'> <C-O>:NERDTreeTabsToggle<CR>

  " Re-open NERDTree at current directory
"  map <D-|> :NERDTree\ .<CR>
"  imap <D-|> <C-O>:NERDTree\ .<CR>

  " Command-T for CtrlP
  macmenu &File.New\ Tab key=<D-T>
  noremap <D-t> :CtrlP<CR>
  inoremap <D-t> <C-O>:CtrlP<CR>

  " Command-/ to toggle comments
  map <D-/> <Plug>CommentaryLine
  vmap <D-/> <Plug>Commentary<CR>gv
  imap <D-/> <C-O><D-/>

  " Split with Command-D and Command-Shift-D
  noremap <D-d> :vsplit<CR>
  inoremap <D-d> <C-O>:vsplit<CR>
  noremap <D-D> :split<CR>
  inoremap <D-D> <C-O>:split<CR>

  " Command-][ to increase/decrease indentation
  vmap <D-]> >gv
  vmap <D-[> <gv

  " Map Command-Digit to switch to tab
  map  <D-0> 0gt
  imap <D-0> <C-O>0gt
  map  <D-1> 1gt
  imap <D-1> <C-O>1gt
  map  <D-2> 2gt
  imap <D-2> <C-O>2gt
  map  <D-3> 3gt
  imap <D-3> <C-O>3gt
  map  <D-4> 4gt
  imap <D-4> <C-O>4gt
  map  <D-5> 5gt
  imap <D-5> <C-O>5gt
  map  <D-6> 6gt
  imap <D-6> <C-O>6gt
  map  <D-7> 7gt
  imap <D-7> <C-O>7gt
  map  <D-8> 8gt
  imap <D-8> <C-O>8gt
  map  <D-9> 9gt
  imap <D-9> <C-O>9gt
endif

let g:nerdtree_tabs_open_on_gui_startup=0

" Start without toolbar
set guioptions-=T

" Scrollbar on the left side is annoying
set guioptions-=L

" Show tab numbers
set guitablabel=%N:\ %M%t

" Disable modeline (security issue)
" This should be the last line
set nomodeline
