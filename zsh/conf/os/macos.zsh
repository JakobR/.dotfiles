#########
# macOS #
#########

# Disable auto-cleanup when upgrading homebrew formulas
export HOMEBREW_NO_INSTALL_CLEANUP=1
# Disable homebrew auto-update
export HOMEBREW_NO_AUTO_UPDATE=1

EMACSCLIENT="${HOMEBREW_PREFIX}/bin/emacsclient"
JR_OPEN="/usr/bin/open"

alias dtw='$HOME/server/dango/transmission-web.sh -f'
alias dtr='$HOME/server/dango/transmission-remote.sh'
alias airport='/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport'
alias pbclear='echo -n | pbcopy'

alias s="/usr/bin/open -a Skim"
compdef '_files -g \*.pdf' s

function dash {
    /usr/bin/open "dash://$1"
}


#################################################################################
# Some functions below are taken from                                           #
# https://github.com/ohmyzsh/ohmyzsh/blob/master/plugins/macos/macos.plugin.zsh #
#################################################################################

# Show/hide hidden files in Finder
# If it doesn't work, try `killall cfprefsd` first (see https://apple.stackexchange.com/a/112091)
alias finder-show-hidden-files='/usr/bin/defaults write com.apple.finder AppleShowAllFiles -bool TRUE  && /usr/bin/killall Finder && /usr/bin/open -g /System/Library/CoreServices/Finder.app'
alias finder-hide-hidden-files='/usr/bin/defaults write com.apple.finder AppleShowAllFiles -bool FALSE && /usr/bin/killall Finder && /usr/bin/open -g /System/Library/CoreServices/Finder.app'

function pfd {
  osascript 2>/dev/null <<EOF
    tell application "Finder"
      return POSIX path of (insertion location as alias)
    end tell
EOF
# Old version:
#   osascript 2>/dev/null <<EOF
#     tell application "Finder"
#       return POSIX path of (target of window 1 as alias)
#     end tell
# EOF
}

function pfs {
  osascript 2>/dev/null <<EOF
    set output to ""
    tell application "Finder" to set the_selection to selection
    set item_count to count the_selection
    repeat with item_index from 1 to count the_selection
      if item_index is less than item_count then set the_delimiter to "\n"
      if item_index is item_count then set the_delimiter to ""
      set output to output & ((item item_index of the_selection as alias)'s POSIX path) & the_delimiter
    end repeat
EOF
}

function cdf {
  cd "$(pfd)"
}
