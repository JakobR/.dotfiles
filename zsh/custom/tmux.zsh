# Is tmux installed?
if type tmux >/dev/null 2>/dev/null; then

  # Only if not inside tmux
  if [[ -z "$TMUX" ]] then

    jr_tmux_sessions=`tmux list-sessions 2>&1`

    # Are there any tmux sessions?
    if [[ ( "$jr_tmux_sessions" != "failed to connect to server"* ) &&
          ( "$jr_tmux_sessions" != "no server running on"* ) &&
          ( "$jr_tmux_sessions" != "error connecting to"* )
       ]] then

      # If yes, list them
      echo "\n\033[4mActive tmux sessions:\033[0m\n$jr_tmux_sessions\n"
    fi

  fi

  # Returns ID of first unattached tmux session (empty if there is none)
  first_unattached_tmux_session () {
    tmux list-sessions -F "#{?session_attached,1,0} #{session_id}" | grep -m 1 '^0 ' | cut -c 3-
  }

  tmux-attach-or-new () {
    unattached_tmux_session=$(first_unattached_tmux_session)
    if [[ -n "$unattached_tmux_session" ]] then
      exec tmux attach -t "$unattached_tmux_session"
    else
      exec tmux new-session
    fi
  }

fi
