if [[ ( "$OSTYPE" =~ ^linux ) || ( -n "${IN_NIX_SHELL:-}") ]] then
    # Aliases for GNU coreutils (they are used in nix-shell even on macOS)
    alias ls="ls -F --color=auto"
    alias ll="ls -lhF --time-style=long-iso"
elif [[ ($OSTYPE =~ ^darwin) || ($OSTYPE =~ ^freebsd) ]] then
    alias ls="ls -FG"
    alias ll="ls -lhF -D '%F %T'"
else
    alias ls='ls -F'
    alias ll='ls -lhF'
fi

alias la="ll -a"
