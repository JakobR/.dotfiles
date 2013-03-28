
alias l='ls -F'

platform=`uname`
if [[ $platform == 'Darwin' || $platform == 'FreeBSD' ]] then
  alias ll='ls -lFhT'
  alias la='ls -laFhT'
else
  alias ll='ls -lFh'
  alias la='ls -laFh'
fi
