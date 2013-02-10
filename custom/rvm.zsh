
# Load RVM into a shell session *as a function*
[[ -s "/usr/local/rvm/scripts/rvm" ]] && source "/usr/local/rvm/scripts/rvm"
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# As described on https://rvm.io/integration/zsh/
__rvm_project_rvmrc
