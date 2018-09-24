set nocompatible                " required
filetype off                    " required

" Initialize vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#begin('~/.vim/bundle')
" TODO: maybe use https://github.com/junegunn/vim-plug instead
" (supports post-install actions for plugins)
" See https://www.reddit.com/r/vim/comments/3thtrv/just_switched_to_vimplug_from_vundle/

" Don't let vundle update itself.
" (since it's a git submodule it's not on a branch, so updating fails)
Plugin 'VundleVim/Vundle.vim', {'pinned': 1}


Plugin 'tpope/vim-git'

if v:version >= 703
  Plugin 'myusuf3/numbers.vim'
end

Plugin 'altercation/vim-colors-solarized'
Plugin 'lifepillar/vim-solarized8'

Plugin 'tpope/vim-markdown'
" Plugin 'cakebaker/scss-syntax.vim'
Plugin 'pangloss/vim-javascript'
Plugin 'exu/pgsql.vim'
" Plugin 'jnwhiteh/vim-golang'
Plugin 'artoj/qmake-syntax-vim'
Plugin 'vim-scripts/abnf'
Plugin 'tmatilai/vim-monit'
Plugin 'adimit/prolog.vim'
Plugin 'niklasl/vim-rdf'
Plugin 'nikolavp/sparql.vim'
Plugin 'keith/swift.vim'
Plugin 'peterhoeg/vim-qml'
Plugin 'pbrisbin/vim-syntax-shakespeare'
" Plugin 'neovimhaskell/haskell-vim'
Plugin 'goolord/vim2hs'
Plugin 'vim-python/python-syntax'
Plugin 'nickhutchinson/vim-cmake-syntax'
Plugin 'c-cube/vim-tptp'
Plugin 'bohlender/vim-z3-smt2'
Plugin 'justinmk/vim-sneak'

Plugin 'LnL7/vim-nix'
" Plugin 'MarcWeber/vim-addon-nix'

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
  " Plugin 'lucapette/vim-textobj-underscore'
  " Plugin 'mattn/vim-textobj-url'
  Plugin 'Julian/vim-textobj-variable-segment'
  " Plugin 'nelstrom/vim-textobj-rubyblock'
  " Plugin 'rbonvall/vim-textobj-latex'   (part of vimtex)
  Plugin 'wellle/targets.vim'

  Plugin 'ctrlpvim/ctrlp.vim'
  Plugin 'kana/vim-altr'
  Plugin 'mileszs/ack.vim'

  Plugin 'rhysd/vim-clang-format'

  Plugin 'chrisbra/csv.vim'

  Plugin 'tpope/vim-dispatch'
  Plugin 'Shougo/vimproc.vim'

  " Plugin 'ervandew/supertab'
  Plugin 'SirVer/ultisnips'
  Plugin 'honza/vim-snippets'

  " disable extended highlighting of matching delimiters (it seems to be messing up the cursor)
  let g:vimtex_matchparen_enabled = 0
  Plugin 'lervag/vimtex'

  " https://github.com/parsonsmatt/intero-neovim
  " Plugin 'eagletmt/ghcmod-vim'
  " Plugin 'eagletmt/neco-ghc'
  " Plugin 'glittershark/vim-hare'
  " Plugin 'autozimu/LanguageClient-neovim'
  " Plugin 'natebosch/vim-lsc'

  " Plugin 'tomtom/tlib_vim'
  " Plugin 'MarcWeber/vim-addon-mw-utils'
  " Plugin 'garbas/vim-snipmate'

  " Plugin 'godlygeek/tabular'

  " Plugin 'vim-latex/vim-latex'
  " Plugin 'xuhdev/vim-latex-live-preview'

  " Plugin 'scrooloose/nerdtree'
  " Plugin 'jistr/vim-nerdtree-tabs'
  " Plugin 'scrooloose/nerdcommenter'

  " Plugin 'Yggdroot/indentLine'
  " Plugin 'nathanaelkane/vim-indent-guides'

  " Installation guide: https://github.com/Valloric/YouCompleteMe
  if has('gui_macvim')
    Plugin 'Valloric/YouCompleteMe'
    " possible alternative:
    " Plugin 'Shougo/neocomplete'

    " Disable syntastic for C++ if we have YCM
    " (TODO: this should be in vimrc, with a check whether YCM is actually loaded and working)
    let g:syntastic_cpp_checkers = []
  endif

  if v:version >= 704 && exists("*matchaddpos")
    " Plugin 'bbchung/clighter'

    let g:clighter_libclang_file = '/Library/Developer/CommandLineTools/usr/lib/libclang.dylib'
  end

  Plugin 'Valloric/MatchTagAlways'

  " Plugin 'sjl/gundo.vim'
  Plugin 'mbbill/undotree'

  Plugin 'rizzatti/dash.vim'

  Plugin 'jceb/vim-orgmode'
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

" Look at http://bling.github.io/blog/2013/06/02/unite-dot-vim-the-plugin-you-didnt-know-you-need/
" Plugin 'Shougo/unite.vim'

" Plugin 'xolox/vim-session'
" Plugin 'tpope/vim-obsession'


" All Plugins must be added before the following line
call vundle#end()               " required
filetype plugin indent on       " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
