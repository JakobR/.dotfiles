set nocompatible               " required!
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'ciaranm/securemodelines'

Bundle 'tpope/vim-git'
Bundle 'tpope/vim-fugitive'
Bundle 'airblade/vim-gitgutter'

Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-speeddating'
Bundle 'tpope/vim-unimpaired'

Bundle 'kien/ctrlp.vim'
Bundle 'myusuf3/numbers.vim'
Bundle 'mileszs/ack.vim'

Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'jistr/vim-nerdtree-tabs'

Bundle 'altercation/vim-colors-solarized'

Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-bundler'
Bundle 'tpope/vim-markdown'
Bundle 'vim-ruby/vim-ruby'
Bundle 'cakebaker/scss-syntax.vim'
Bundle 'pangloss/vim-javascript'

Bundle 'vim-scripts/abnf'
Bundle 'tmatilai/vim-monit'

" Disable these bundles until I have time to configure them properly
"Bundle 'scrooloose/syntastic'
"Bundle 'Shougo/neocomplcache'

" More interesting bundles to check out:
" https://github.com/tpope/vim-commentary
" https://github.com/tomtom/tcomment_vim
" Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
" Bundle 'kana/vim-smartinput'
" Bundle 'mattn/zencoding-vim'
" https://github.com/terryma/vim-multiple-cursors

filetype plugin indent on     " required!
