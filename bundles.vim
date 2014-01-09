set nocompatible               " required!
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Don't let vundle manage itself.
" (since it's a git submodule it's not on a branch, so updating fails)
"Bundle 'gmarik/vundle'

Bundle 'ciaranm/securemodelines'

Bundle 'tpope/vim-git'
Bundle 'tpope/vim-fugitive'
Bundle 'airblade/vim-gitgutter'

Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-speeddating'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-eunuch'
Bundle 'tpope/vim-commentary'

Bundle 'scrooloose/syntastic'

Bundle 'kana/vim-textobj-user'
Bundle 'kana/vim-textobj-indent'
Bundle 'lucapette/vim-textobj-underscore'
Bundle 'mattn/vim-textobj-url'
Bundle 'Julian/vim-textobj-variable-segment'
Bundle 'nelstrom/vim-textobj-rubyblock'
Bundle 'rbonvall/vim-textobj-latex'

Bundle 'kien/ctrlp.vim'
Bundle 'rking/ag.vim'
Bundle 'mileszs/ack.vim'

if v:version >= 703
  Bundle 'myusuf3/numbers.vim'
end

Bundle 'scrooloose/nerdtree'
Bundle 'jistr/vim-nerdtree-tabs'

Bundle 'altercation/vim-colors-solarized'

Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-bundler'
Bundle 'tpope/vim-markdown'
Bundle 'vim-ruby/vim-ruby'
Bundle 'cakebaker/scss-syntax.vim'
Bundle 'pangloss/vim-javascript'
Bundle 'exu/pgsql.vim'
Bundle 'lukerandall/haskellmode-vim'

" good, but sadly too slow
"Bundle 'dag/vim2hs'

Bundle 'vim-scripts/abnf'
Bundle 'tmatilai/vim-monit'

" Disable these bundles until I have time to configure them properly
"Bundle 'Shougo/neocomplcache'

" More interesting bundles to check out:
" https://github.com/tpope/vim-commentary
" https://github.com/tomtom/tcomment_vim
" Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
" Bundle 'kana/vim-smartinput'
" Bundle 'mattn/zencoding-vim'
" https://github.com/terryma/vim-multiple-cursors

filetype plugin indent on     " required!
