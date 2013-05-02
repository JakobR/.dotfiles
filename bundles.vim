set nocompatible               " required!
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'tpope/vim-git'
Bundle 'tpope/vim-fugitive'
Bundle 'airblade/vim-gitgutter'

Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-speeddating'

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
Bundle 'othree/html5.vim'

" Disable these bundles until I have time to configure them properly
"Bundle 'scrooloose/syntastic'
"Bundle 'Shougo/neocomplcache'

" More interesting bundles to check out:
" https://github.com/tpope/vim-commentary
" https://github.com/tomtom/tcomment_vim
" https://github.com/tpope/vim-unimpaired
" Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}

filetype plugin indent on     " required!
