set lines=50
set columns=120

set guifont=Menlo:h14

set background=light
colorscheme solarized

" from https://github.com/raecoo/dotfiles/blob/master/gvimrc
if has("gui_macvim")
  " Cmd-\ toggles NERDTree
  map <D-\> :NERDTreeTabsToggle<CR>
  imap <D-\> <Esc>:NERDTreeTabsToggle<CR>
  " Cmd-\ is frequently broken, but I am unable to reproduce it consistently
  " Use Cmd=' as alternative
  map <D-'> :NERDTreeTabsToggle<CR>
  imap <D-'> <Esc>:NERDTreeTabsToggle<CR>

  " Re-open NERDTree at current directory
"  map <D-|> :NERDTree\ .<CR>
"  imap <D-|> <Esc>:NERDTree\ .<CR>

  " Command-T for CtrlP
  macmenu &File.New\ Tab key=<D-T>
  map <D-t> :CtrlP<CR>
  imap <D-t> <Esc>:CtrlP<CR>

  " Command-/ to toggle comments
  nmap <D-/> gcc
  vmap <D-/> gcgv
  imap <D-/> <Esc>gcci
  " map <D-/> <plug>NERDCommenterToggle<CR>
  " imap <D-/> <Esc><plug>NERDCommenterToggle<CR>i

  " Command-][ to increase/decrease indentation
  vmap <D-]> >gv
  vmap <D-[> <gv

  " Map Command-Digit to switch to tab
  map  <D-0> 0gt
  imap <D-0> <Esc>0gt
  map  <D-1> 1gt
  imap <D-1> <Esc>1gt
  map  <D-2> 2gt
  imap <D-2> <Esc>2gt
  map  <D-3> 3gt
  imap <D-3> <Esc>3gt
  map  <D-4> 4gt
  imap <D-4> <Esc>4gt
  map  <D-5> 5gt
  imap <D-5> <Esc>5gt
  map  <D-6> 6gt
  imap <D-6> <Esc>6gt
  map  <D-7> 7gt
  imap <D-7> <Esc>7gt
  map  <D-8> 8gt
  imap <D-8> <Esc>8gt
  map  <D-9> 9gt
  imap <D-9> <Esc>9gt
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
