if type ag >/dev/null 2>/dev/null; then

    # Usually I want to search case-insensitively.
    # Use \ag (prefixed with backslash) for case sensitive search.
    alias ag='ag --ignore-case'

fi

# alias grep="grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}"
alias grep="grep --color=auto"

# Some linux distributions install 'fd' as 'fdfind'
if (( $+commands[fdfind] )) then
    alias fd=fdfind
fi
