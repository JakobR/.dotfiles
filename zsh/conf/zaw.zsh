# zaw configuration

bindkey '^R' zaw-history
bindkey -M filterselect '^R' down-line-or-history
bindkey -M filterselect '^S' up-line-or-history
bindkey -M filterselect '^E' accept-search

#zstyle ':filter-select:highlight' matched fg=green
zstyle ':filter-select' max-lines 15
zstyle ':filter-select' rotate-list yes
zstyle ':filter-select' case-insensitive yes
zstyle ':filter-select' extended-search yes # see https://github.com/zsh-users/zaw for explanation
