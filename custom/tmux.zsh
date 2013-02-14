
# Only if not inside tmux
if [[ -z "$TMUX" ]] then

  jr_tmux_sessions=`tmux list-sessions 2>&1`

  # Are there any tmux sessions?
  if [[ "$jr_tmux_sessions" != "failed to connect to server" ]] then

    # If yes, list them
    echo "\n\033[4mActive tmux sessions:\033[0m\n$jr_tmux_sessions\n"
  fi

fi

# I often forget the "-t"
alias tmux-attach='tmux attach -t'
