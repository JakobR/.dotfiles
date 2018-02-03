set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc
if exists("g:gui_oni") || exists("neovim_dot_app") || has('gui_vimr')
    source ~/.config/nvim/ginit.vim
endif
