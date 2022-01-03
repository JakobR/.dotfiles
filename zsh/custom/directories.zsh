# Changing/making/removing directory
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus

# NOTE: global aliases (-g) are expanded even if they are not in command position
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

alias 1='cd -'
alias 2='cd -2'
alias 3='cd -3'
alias 4='cd -4'
alias 5='cd -5'
alias 6='cd -6'
alias 7='cd -7'
alias 8='cd -8'
alias 9='cd -9'

function d () {
  if [[ -n $1 ]]; then
    dirs "$@"
  else
    dirs -v | head -10
  fi
}
compdef _dirs d

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
