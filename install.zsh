#!/usr/bin/env zsh

# This script should be idempotent (running it multiple times should result in the same effect).
# If an error occurs, fix the problem according to the message and run the script again.


# Abort on error
set -e


# Colors
autoload -Uz colors
colors

function echo_msg () {
  echo "$fg[green]$@$fg[default]"
}
function echo_err () {
  echo "$fg[red]$@$fg[default]"
}
function echo_path () {
  # Resetting color to default isn't actually correct (if used with echo_err for example), but that doesn't really matter
  echo "$fg[yellow]$@$fg[default]"
}


# Parse command line parameters.
if [[ ("${#}" -eq "1") && ("$1" = "--update") ]] then
  JR_UPDATE="true"
elif [[ "${#}" -ne "0" ]] then
  echo_err "Invalid number of arguments!"
  echo "\nUsage:\n  $0 [--update]"
  exit 1
fi


DEFAULT_JR_DOTFILES="$HOME/.dotfiles"
if [[ -z "$JR_DOTFILES" ]] then
  # Use default location if not set
  JR_DOTFILES="$DEFAULT_JR_DOTFILES"
  if [[ -L "$JR_DOTFILES" ]] then
    JR_DOTFILES=$(readlink -n -- "$JR_DOTFILES")
  fi
  export JR_DOTFILES
fi

echo_msg "Installation directory: $(echo_path "$JR_DOTFILES")"

if [[ ! -d "$JR_DOTFILES" ]] then
  echo_msg "Directory doesn't exist yet, cloning from GitHub..."
  git clone --recursive 'https://github.com/JakobR/.dotfiles.git' "$JR_DOTFILES"
fi

if [[ ! -f "$JR_DOTFILES/.jr_dotfiles" ]] then
  echo_err "JR_DOTFILES directory (path: '$JR_DOTFILES') does not contain expected data!"
  exit 1
fi


# creates a symlink with additional checks and output
function create_symlink () {
  local orig_path="$1"
  local link_path="$2"

  if [[ ! -e "$orig_path" ]] then
    echo_err "Error: Original file does not exist: $orig_path"
    exit 3
  fi

  # Link exists. Is it the correct one?
  if [[ (-L "$link_path") && ("$(readlink -n -- $link_path)" = "$orig_path") ]] then
    echo "Found correct link at $(echo_path $link_path)"
  else
    # Something else exists at original path. Move it out of the way.
    if [[ -e "$link_path" ]] then
      # Find a free file name for the backup file
      local i=0
      local backup_base="$link_path.original"
      local backup_path="$backup_base"
      while [[ -e "$backup_path" ]] do
        (( i = i + 1 ))
        backup_path="$backup_base.$i"
      done
      # Try to move
      echo "File already exists. Moving existing $(echo_path $link_path) to $(echo_path $backup_path)"
      mv -n -- "$link_path" "$backup_path"
      # Did it work?
      if [[ -e "$link_path" ]] then
        echo_err "Failed. Please resolve manually by deleting or moving $(echo_path $link_path)"
        exit 4
      fi
    fi
    # Create link
    echo "Creating link from $(echo_path $link_path) to $(echo_path $orig_path)..."
    /bin/ln -is -- "$orig_path" "$link_path"
  fi
}

# Expects a path relative to $JR_DOTFILES.
function create_symlink_to_home () {
  local file="$1"

  # "${file:t}" is the basename of the path in $file
  create_symlink "$JR_DOTFILES/$file" "$HOME/.${file:t}"
}


if [[ "$JR_DOTFILES" != "$DEFAULT_JR_DOTFILES" ]] then
  echo_msg "Symlinking $HOME/.dotfiles..."
  create_symlink "$JR_DOTFILES" "$DEFAULT_JR_DOTFILES"
fi

echo_msg "Symlinking dotfiles..."
create_symlink_to_home 'zsh/zshrc'
create_symlink_to_home 'zsh/zlogout'
create_symlink_to_home 'zsh/zprofile'
create_symlink_to_home 'tmux.conf'
create_symlink_to_home 'vim'
create_symlink_to_home 'vimrc'
create_symlink_to_home 'gvimrc'
create_symlink_to_home 'ackrc'
create_symlink_to_home 'gitconfig'
create_symlink_to_home 'gitignore-global'
create_symlink_to_home 'ghci'
create_symlink_to_home 'inputrc'
create_symlink_to_home 'gemrc'
create_symlink_to_home 'Xmodmap'
mkdir -p "${HOME}/.config/mpv"
create_symlink "${JR_DOTFILES}/mpv/input.conf" "${HOME}/.config/mpv/input.conf"
create_symlink "${JR_DOTFILES}/mpv/mpv.conf" "${HOME}/.config/mpv/mpv.conf"
mkdir -p "${HOME}/.cabal"
create_symlink "${JR_DOTFILES}/cabal-config" "${HOME}/.cabal/config"
mkdir -p "${HOME}/.stack/global-project"
create_symlink "${JR_DOTFILES}/stack/config.yaml" "${HOME}/.stack/config.yaml"
create_symlink "${JR_DOTFILES}/stack/global-project/stack.yaml" "${HOME}/.stack/global-project/stack.yaml"


if [[ "$OSTYPE" =~ ^darwin ]] then
  echo_msg "Symlinking OS X specific files..."
  create_symlink_to_home 'xvimrc'
  create_symlink_to_home 'hammerspoon'
  create_symlink "$JR_DOTFILES/Karabiner/private.xml" "$HOME/Library/Application Support/Karabiner/private.xml"
  create_symlink "$JR_DOTFILES/Ukelele/US_with_umlauts.keylayout" "$HOME/Library/Keyboard Layouts/US_with_umlauts.keylayout"

  echo_msg "Running OS X configuration script..."
  "$JR_DOTFILES"/osx.sh

  echo_msg "Configuring Seil..."
  SEIL="/Applications/Seil.app/Contents/Library/bin/seil"
  if [[ (-f "$SEIL") && (-x "$SEIL") ]] then
    # Map Caps Lock to 80, which is the key code for F19.
    "$SEIL" set keycode_capslock 80
    "$SEIL" set enable_capslock 1
  else
    echo_err "Unable to configure Seil. Please ensure you have at least version 10.9.0 installed, and run this script again."
    # Not serious enough to exit...
  fi
fi


# vim bundles
echo_msg "Installing vim bundles..."
vim -u "$JR_DOTFILES/bundles.vim" '+PluginInstall' '+qall'


if [[ "$JR_UPDATE" = "true" ]] then
  echo_msg "Updating..."
  (
    set -e
    cd "$JR_DOTFILES"
    git pull origin master
    git submodule sync
    git submodule update --init --recursive
    vim -u "$JR_DOTFILES/bundles.vim" '+PluginUpdate'
  )
fi


# Permissions
echo_msg "Updating permissions..."
chmod -- go-rwx "$HOME"

mkdir -p -- "$HOME"/.ssh
chmod -R -- go-rwx "$HOME"/.ssh

mkdir -p -- "$HOME"/.vim-tmp
chmod -- go-rwx "$HOME"/.vim-tmp

chmod -R -- go-w "$JR_DOTFILES"


echo
echo_msg "Done! Installation completed successfully."

echo
cat <<END
Additional configuration tasks to be performed manually:

  * If you want to connect to this machine via ssh, sshd should accept the TERM_PROGRAM variable.
    Add this line to sshd_config:

        AcceptEnv TERM_PROGRAM

END
if [[ "$OSTYPE" =~ ^darwin ]] then
  cat <<END
  * Install additional software:
    - Seil (formerly PCKeyboardHack)
          https://pqrs.org/osx/karabiner/seil.html
    - Karabiner (formerly KeyRemap4MacBook)
          https://pqrs.org/osx/karabiner/index.html.en
    - Slate
          https://github.com/jigish/slate

  * In System Preferences / Keyboard / Modifier Keys, set Caps Lock to "No Action".
    (this is necessary for Seil's KeyOverlaidModifier to work correctly.)

  * In Karabiner, enable the mappings.

END
fi
