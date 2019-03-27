" Better indendation for C++11 lambdas, via https://stackoverflow.com/a/26662103
setlocal cindent
setlocal cinoptions=j1,(0,ws,Ws

" Don't indent namespace content
setlocal cinoptions+=N-s

" Don't indent function return types if they are on a separate line
setlocal cinoptions+=t0

" TODO: fix template indentation
" See https://stackoverflow.com/questions/387792/vim-indentation-for-c-templates for ideas
