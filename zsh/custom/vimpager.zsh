
# Use vimpager from submodule if not installed
type vimpager >/dev/null 2>/dev/null || alias vimpager="$HOME/.dotfiles/vimpager/vimpager"
type vimcat   >/dev/null 2>/dev/null || alias   vimcat="$HOME/.dotfiles/vimpager/vimcat"

export PAGER='vimpager'
