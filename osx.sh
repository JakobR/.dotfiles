#!/bin/sh


echo "osx.sh disabled"
exit 1


# OS X Settings
# most stuff from https://github.com/sjl/dotfiles/blob/master/osx.sh
# also interesting: https://github.com/mathiasbynens/dotfiles/blob/master/.osx

# Disable menu bar transparency
defaults write NSGlobalDomain AppleEnableMenuBarTransparency -bool false

# Save to disk (not to iCloud) by default
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

# Expand save panel by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true

# Expand print panel by default
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true

# Display ASCII control characters using caret notation in standard text views
# Try e.g. `cd /tmp; unidecode "\x{0000}" > cc.txt; open -e cc.txt`
defaults write NSGlobalDomain NSTextShowsControlCharacters -bool true

# Disable emoji substitution in Messages
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "automaticEmojiSubstitutionEnablediMessage" -bool false

# Disable opening and closing window animations
defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false

# Enable subpixel font rendering on non-Apple LCDs
defaults write NSGlobalDomain AppleFontSmoothing -int 2

# Disable auto-correct
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

# Require password immediately after sleep or screen saver begins
defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -int 0

# Allow text selection in Quick Look window
# defaults write com.apple.finder QLEnableTextSelection -bool true
# This setting ruins Quick Look, see http://apple.stackexchange.com/questions/126065/quick-look-sometimes-shows-blank-panel-in-mavericks
# Remove the setting with: defaults delete com.apple.finder QLEnableTextSelection

# Show status bar in Finder
defaults write com.apple.finder ShowStatusBar -bool true

# Automatically open a new Finder window when a volume is mounted
defaults write com.apple.frameworks.diskimages auto-open-ro-root -bool true
defaults write com.apple.frameworks.diskimages auto-open-rw-root -bool true
defaults write com.apple.finder OpenWindowForNewRemovableDisk -bool true

# Display full POSIX path as Finder window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

# Disable the warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Show item info below desktop icons
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist

# Enable snap-to-grid for desktop icons
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:arrangeBy kind" ~/Library/Preferences/com.apple.finder.plist

# Disable the warning before emptying the Trash
#defaults write com.apple.finder WarnOnEmptyTrash -bool false

# Show the ~/Library folder
chflags nohidden ~/Library

# Enable highlight hover effect for the grid view of a stack (Dock)
defaults write com.apple.dock mouse-over-hilte-stack -bool true

# Show indicator lights for open applications in the Dock
defaults write com.apple.dock show-process-indicators -bool true

# Make Dock icons of hidden applications translucent
defaults write com.apple.dock showhidden -bool true

# Disable Safari’s thumbnail cache for History and Top Sites
defaults write com.apple.Safari DebugSnapshotsUpdatePolicy -int 2

# Enable Safari’s debug menu
defaults write com.apple.Safari IncludeInternalDebugMenu -bool true

# Make Safari’s search banners default to Contains instead of Starts With
defaults write com.apple.Safari FindOnPageMatchesWordStartsOnly -bool false

# Don't open PDF files in Safari
defaults write com.apple.Safari WebKitOmitPDFSupport -bool YES

# Safari shouldn't open "safe" downloads automatically
defaults write com.apple.Safari AutoOpenSafeDownloads -bool NO

# Add a context menu item for showing the Web Inspector in web views
defaults write NSGlobalDomain WebKitDeveloperExtras -bool true

# Enable the debug menu in Address Book
defaults write com.apple.addressbook ABShowDebugMenu -bool true

# Enable the debug menu in iCal
defaults write com.apple.iCal IncludeDebugMenu -bool true

# Copy email addresses as `foo@example.com` instead of `Foo Bar <foo@example.com>` in Mail.app
defaults write com.apple.mail AddressesIncludeNameOnPasteboard -bool false

# Prefer plain text emails in Mail.app
# (apparently there's no way to completely disable HTML in emails)
defaults write com.apple.mail PreferPlainText -bool true

# Disable bouncing effect of icons in the Dock
defaults write com.apple.dock no-bouncing -bool true

# Xcode scrolling (to work around an XVim bug)
# See https://github.com/XVimProject/XVim/issues/882
defaults write com.apple.dt.Xcode AppleShowScrollBars -string "WhenScrolling"

# Fix annoying scrollbar flickering while typing in OneNote
defaults write com.microsoft.onenote.mac AppleShowScrollBars -string "WhenScrolling"

# Don't show the "Other..." option on the login window
sudo defaults write /Library/Preferences/com.apple.loginwindow SHOWOTHERUSERS_MANAGED -bool FALSE

# Speed up browsing on network shares, see https://support.apple.com/en-us/HT208209
# defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool TRUE
