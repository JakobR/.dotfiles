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
" alternative to gitgutter: https://github.com/mhinz/vim-signify

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
Bundle 'kana/vim-altr'
Bundle 'rking/ag.vim'
Bundle 'mileszs/ack.vim'

if v:version >= 703
  Bundle 'myusuf3/numbers.vim'
end

Bundle 'scrooloose/nerdtree'
Bundle 'jistr/vim-nerdtree-tabs'

Bundle 'altercation/vim-colors-solarized'

" Bundle 'Yggdroot/indentLine'
" Bundle 'nathanaelkane/vim-indent-guides'

Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-bundler'
Bundle 'tpope/vim-markdown'
Bundle 'vim-ruby/vim-ruby'
Bundle 'cakebaker/scss-syntax.vim'
Bundle 'pangloss/vim-javascript'
Bundle 'exu/pgsql.vim'
Bundle 'lukerandall/haskellmode-vim'
Bundle 'jnwhiteh/vim-golang'

" good, but sadly too slow
"Bundle 'dag/vim2hs'

Bundle 'Mizuchi/STL-Syntax'
Bundle 'vim-jp/cpp-vim'
Bundle 'artoj/qmake-syntax-vim'
Bundle 'vim-scripts/abnf'
Bundle 'tmatilai/vim-monit'

" Installation guide: https://github.com/Valloric/YouCompleteMe
if has('gui_macvim')
  Bundle 'Valloric/YouCompleteMe'

  " Disable syntastic for C++ if we have YCM
  " (TODO: this should be in vimrc, with a check whether YCM is actually loaded and working)
  let g:syntastic_cpp_checkers = []
endif

if has('python')
  Bundle 'Valloric/MatchTagAlways'
end

" Bundle 'sjl/gundo.vim'
Bundle 'mbbill/undotree'

" More interesting bundles to check out:
" Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
" Bundle 'kana/vim-smartinput'
" Bundle 'mattn/zencoding-vim'
" https://github.com/terryma/vim-multiple-cursors
" https://github.com/bling/vim-airline
"
" https://github.com/Lokaltog/vim-easymotion
" or https://github.com/haya14busa/vim-easymotion

" https://github.com/suan/vim-instant-markdown

" snippets: https://github.com/SirVer/ultisnips

" Look at http://bling.github.io/blog/2013/06/02/unite-dot-vim-the-plugin-you-didnt-know-you-need/
" Bundle 'Shougo/unite.vim'

" Bundle 'xolox/vim-session'
" Bundle 'tpope/vim-obsession'

filetype plugin indent on     " required!
