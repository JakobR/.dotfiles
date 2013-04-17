
# Use vimpager instead of less
export PAGER='vimpager'

# Use vimpager from submodule if not installed
type vimpager >/dev/null 2>/dev/null
if [[ $? -ne 0 ]] then
  alias vimpager="$HOME/.dotfiles/vimpager/vimpager"
  export PAGER="$HOME/.dotfiles/vimpager/vimpager"
fi

type vimcat >/dev/null 2>/dev/null || alias vimcat="$HOME/.dotfiles/vimpager/vimcat"
