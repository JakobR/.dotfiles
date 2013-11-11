# Shortcut for paging
alias p='$PAGER'

# Use vimpager instead of less
export PAGER='vimpager'

# Use vimpager from submodule if not installed
type vimpager >/dev/null 2>/dev/null
if [[ $? -ne 0 ]] then
  alias vimpager="$JR_DOTFILES/vimpager/vimpager"
  export PAGER="$JR_DOTFILES/vimpager/vimpager"
fi

type vimcat >/dev/null 2>/dev/null || alias vimcat="$JR_DOTFILES/vimpager/vimcat"
