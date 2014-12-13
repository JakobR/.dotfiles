rvm () {
  unset -f rvm
  echo "Loading rvm..."

  # Possible rvm install paths
  jr_rvm_paths=("$HOME/.rvm" "/usr/local/rvm")

  for jr_rvm_path in $jr_rvm_paths
  do
    # rvm exists at that path?
    if [[ -s "$jr_rvm_path/scripts/rvm" ]] then
      # Add RVM to PATH for scripting
      PATH="$PATH:$jr_rvm_path/bin"
      # Load RVM into a shell session *as a function*
      source "$jr_rvm_path/scripts/rvm"
      # As described on https://rvm.io/integration/zsh/
      __rvm_project_rvmrc
      # Don't check other paths (first found wins)
      break
    fi
  done

  # If we got arguments, pass them to rvm
  if (( $# > 0 )) then
    rvm "$@"
  fi
}
