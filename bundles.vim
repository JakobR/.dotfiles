set nocompatible               " required!
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Don't let vundle manage itself.
" (since it's a git submodule it's not on a branch, so updating fails)
"Bundle 'gmarik/vundle'


Bundle 'tpope/vim-git'

if v:version >= 703
  Bundle 'myusuf3/numbers.vim'
end

Bundle 'altercation/vim-colors-solarized'

Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-bundler'
Bundle 'tpope/vim-markdown'
Bundle 'vim-ruby/vim-ruby'
Bundle 'cakebaker/scss-syntax.vim'
Bundle 'pangloss/vim-javascript'
Bundle 'exu/pgsql.vim'
Bundle 'jnwhiteh/vim-golang'
Bundle 'artoj/qmake-syntax-vim'
Bundle 'vim-scripts/abnf'
Bundle 'tmatilai/vim-monit'
Bundle 'adimit/prolog.vim'
Bundle 'niklasl/vim-rdf'
Bundle 'nikolavp/sparql.vim'
Bundle 'Keithbsmiley/swift.vim'
Bundle 'peterhoeg/vim-qml'

" C++
" Bundle 'vim-jp/cpp-vim'
" Bundle 'Mizuchi/STL-Syntax'
Bundle 'octol/vim-cpp-enhanced-highlight'

if !exists('g:vimpager')
  " Don't need these plugins in vimpager
  Bundle 'JakobR/securemodelines'

  Bundle 'tpope/vim-fugitive'
  Bundle 'airblade/vim-gitgutter'
  " alternative to gitgutter: https://github.com/mhinz/vim-signify

  Bundle 'tpope/vim-repeat'
  Bundle 'tpope/vim-surround'
  Bundle 'tpope/vim-speeddating'
  Bundle 'tpope/vim-unimpaired'
  Bundle 'tpope/vim-eunuch'
  Bundle 'tpope/vim-commentary'
  Bundle 'terryma/vim-expand-region'

  Bundle 'scrooloose/syntastic'

  Bundle 'aperezdc/vim-template'

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

  Bundle 'rhysd/vim-clang-format'

  Bundle 'Shougo/vimproc.vim'
  " Bundle 'eagletmt/ghcmod-vim'
  Bundle 'bitc/vim-hdevtools'
  Bundle 'eagletmt/neco-ghc'
  " Bundle 'lukerandall/haskellmode-vim'
  Bundle 'kana/vim-filetype-haskell'

  " Bundle 'scrooloose/nerdtree'
  " Bundle 'jistr/vim-nerdtree-tabs'

  " Bundle 'Yggdroot/indentLine'
  " Bundle 'nathanaelkane/vim-indent-guides'

  " Installation guide: https://github.com/Valloric/YouCompleteMe
  if has('gui_macvim')
    Bundle 'Valloric/YouCompleteMe'

    " Disable syntastic for C++ if we have YCM
    " (TODO: this should be in vimrc, with a check whether YCM is actually loaded and working)
    let g:syntastic_cpp_checkers = []
    " Disable python-mode's autocompletion if we have YCM
    let g:pymode_rope_complete_on_dot = 0
  endif

  if v:version >= 704 && exists("*matchaddpos")
    " Bundle 'bbchung/clighter'

    let g:clighter_libclang_file = '/Library/Developer/CommandLineTools/usr/lib/libclang.dylib'
  end

  if has('python')
    Bundle 'Valloric/MatchTagAlways'

    "Bundle 'klen/python-mode'
    " workaround for freezes, see https://github.com/klen/python-mode/issues/342
    let g:pymode_rope_lookup_project = 0
  end

  " Bundle 'sjl/gundo.vim'
  Bundle 'mbbill/undotree'
end

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
