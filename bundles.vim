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

" good, but sadly too slow
"Bundle 'dag/vim2hs'

Bundle 'vim-scripts/abnf'
Bundle 'tmatilai/vim-monit'

" Installation guide: https://github.com/Valloric/YouCompleteMe
if has('gui_macvim')
  " MacVim links to system python, even if homebrew python is installed
  " (check with `otool -L /usr/local/Cellar/macvim/7.4-72/MacVim.app/Contents/MacOS/Vim|grep -i python`)
  " YCM, on the other hand, seems to use python from $PATH (and the server crashes)
  " workaround: Set path to python explicitly to avoid having to unlink
  " homebrew's python.
  let g:ycm_path_to_python_interpreter = '/System/Library/Frameworks/Python.framework/Versions/2.7/bin/python'
  Bundle 'Valloric/YouCompleteMe'
endif

Bundle 'Valloric/MatchTagAlways'

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

" snippets: https://github.com/SirVer/ultisnips

filetype plugin indent on     " required!
