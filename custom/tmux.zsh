
# Is tmux installed?
if type tmux >/dev/null 2>/dev/null; then

  # Only if not inside tmux
  if [[ -z "$TMUX" ]] then

    jr_tmux_sessions=`tmux list-sessions 2>&1`

    # Are there any tmux sessions?
    if [[ "$jr_tmux_sessions" != "failed to connect to server"* ]] then

      # If yes, list them
      echo "\n\033[4mActive tmux sessions:\033[0m\n$jr_tmux_sessions\n"
    fi

  fi

  # Attaches to the first unattached session. If there is no unattached session, creates a new one.
  alias tmux-attach-or-new='jr_tmux_session=`tmux list-sessions -F "#{session_name} a=#{session_attached}" 2>/dev/null | grep " a=0\$" | head -n 1`; tmux attach -t ${jr_tmux_session% a=0} 2>/dev/null || tmux; unset jr_tmux_session'

fi
