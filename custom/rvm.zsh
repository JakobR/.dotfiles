
# Possible rvm install paths
jr_rvm_paths=("$HOME/.rvm/scripts/rvm" "/usr/local/rvm/scripts/rvm")

for jr_rvm_path in $jr_rvm_paths
do
  # rvm exists at that path?
  if [[ -s "$jr_rvm_path" ]] then
    # Load RVM into a shell session *as a function*
    source "$jr_rvm_path"
    # As described on https://rvm.io/integration/zsh/
    __rvm_project_rvmrc
    # Don't check other paths (first found wins)
    break
  fi
done
