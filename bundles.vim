set nocompatible                " required
filetype off                    " required

" Initialize vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#begin('~/.vim/bundle')

" Don't let vundle update itself.
" (since it's a git submodule it's not on a branch, so updating fails)
Plugin 'VundleVim/Vundle.vim', {'pinned': 1}


Plugin 'tpope/vim-git'

if v:version >= 703
  Plugin 'myusuf3/numbers.vim'
end

Plugin 'altercation/vim-colors-solarized'

Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-bundler'
Plugin 'tpope/vim-markdown'
Plugin 'vim-ruby/vim-ruby'
Plugin 'cakebaker/scss-syntax.vim'
Plugin 'pangloss/vim-javascript'
Plugin 'exu/pgsql.vim'
Plugin 'jnwhiteh/vim-golang'
Plugin 'artoj/qmake-syntax-vim'
Plugin 'vim-scripts/abnf'
Plugin 'tmatilai/vim-monit'
Plugin 'adimit/prolog.vim'
Plugin 'niklasl/vim-rdf'
Plugin 'nikolavp/sparql.vim'
Plugin 'Keithbsmiley/swift.vim'
Plugin 'peterhoeg/vim-qml'
Plugin 'hdima/python-syntax'

" C++
" Plugin 'vim-jp/cpp-vim'
" Plugin 'Mizuchi/STL-Syntax'
Plugin 'octol/vim-cpp-enhanced-highlight'

if !exists('g:vimpager')
  " Don't need these plugins in vimpager
  Plugin 'JakobR/securemodelines'
  Plugin 'gmarik/sudo-gui.vim'

  Plugin 'tpope/vim-fugitive'
  Plugin 'gregsexton/gitv'
  Plugin 'airblade/vim-gitgutter'
  " alternative to gitgutter: https://github.com/mhinz/vim-signify

  Plugin 'tpope/vim-repeat'
  Plugin 'tpope/vim-surround'
  Plugin 'tpope/vim-speeddating'
  Plugin 'tpope/vim-unimpaired'
  Plugin 'tpope/vim-eunuch'
  Plugin 'tpope/vim-commentary'
  Plugin 'terryma/vim-expand-region'

  Plugin 'scrooloose/syntastic'

  Plugin 'aperezdc/vim-template'

  Plugin 'kana/vim-textobj-user'
  Plugin 'kana/vim-textobj-indent'
  Plugin 'lucapette/vim-textobj-underscore'
  Plugin 'mattn/vim-textobj-url'
  Plugin 'Julian/vim-textobj-variable-segment'
  Plugin 'nelstrom/vim-textobj-rubyblock'
  Plugin 'rbonvall/vim-textobj-latex'

  Plugin 'kien/ctrlp.vim'
  Plugin 'kana/vim-altr'
  Plugin 'mileszs/ack.vim'

  Plugin 'rhysd/vim-clang-format'

  Plugin 'chrisbra/csv.vim'

  Plugin 'tpope/vim-dispatch'
  Plugin 'Shougo/vimproc.vim'
  " Plugin 'eagletmt/ghcmod-vim'
  Plugin 'bitc/vim-hdevtools'
  " Plugin 'eagletmt/neco-ghc'
  " Plugin 'lukerandall/haskellmode-vim'
  " Plugin 'kana/vim-filetype-haskell'
  " Plugin 'dag/vim2hs'

  " Plugin 'git://git.code.sf.net/p/vim-latex/vim-latex'

  " Plugin 'scrooloose/nerdtree'
  " Plugin 'jistr/vim-nerdtree-tabs'

  " Plugin 'Yggdroot/indentLine'
  " Plugin 'nathanaelkane/vim-indent-guides'

  " Installation guide: https://github.com/Valloric/YouCompleteMe
  if has('gui_macvim')
    Plugin 'Valloric/YouCompleteMe'

    " Disable syntastic for C++ if we have YCM
    " (TODO: this should be in vimrc, with a check whether YCM is actually loaded and working)
    let g:syntastic_cpp_checkers = []
    " Disable python-mode's autocompletion if we have YCM
    let g:pymode_rope_complete_on_dot = 0
  endif

  if v:version >= 704 && exists("*matchaddpos")
    " Plugin 'bbchung/clighter'

    let g:clighter_libclang_file = '/Library/Developer/CommandLineTools/usr/lib/libclang.dylib'
  end

  if has('python')
    Plugin 'Valloric/MatchTagAlways'

    "Plugin 'klen/python-mode'
    " workaround for freezes, see https://github.com/klen/python-mode/issues/342
    let g:pymode_rope_lookup_project = 0
  end

  " Plugin 'sjl/gundo.vim'
  Plugin 'mbbill/undotree'

  Plugin 'rizzatti/dash.vim'
end

" More interesting bundles to check out:
" Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
" Plugin 'kana/vim-smartinput'
" Plugin 'mattn/zencoding-vim'
" https://github.com/terryma/vim-multiple-cursors
" https://github.com/bling/vim-airline
"
" https://github.com/Lokaltog/vim-easymotion
" or https://github.com/haya14busa/vim-easymotion

" https://github.com/suan/vim-instant-markdown

" snippets: https://github.com/SirVer/ultisnips

" Look at http://bling.github.io/blog/2013/06/02/unite-dot-vim-the-plugin-you-didnt-know-you-need/
" Plugin 'Shougo/unite.vim'

" Plugin 'xolox/vim-session'
" Plugin 'tpope/vim-obsession'


" All Plugins must be added before the following line
call vundle#end()               " required
filetype plugin indent on       " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
