alias l='ls -F'

if [[ ($OSTYPE =~ ^darwin) || ($OSTYPE =~ ^freebsd) ]] then
  alias ll='ls -lFhT'
  alias la='ls -laFhT'
else
  alias ll='ls -lFh'
  alias la='ls -laFh'
fi
