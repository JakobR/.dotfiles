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
    JR_DOTFILES=$(readlink -n -- $JR_DOTFILES)
  fi
  export JR_DOTFILES
fi

echo_msg "Installation directory: $fg[yellow]$JR_DOTFILES"

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

  if [[ -e "$link_path" ]] then
    # Link exists. Is it the correct one?
    if [[ (-L "$link_path") && ("$(readlink -n -- $link_path)" = "$orig_path") ]] then
      echo "Found correct link at $link_path"
    else
      # TODO: Can backup automatically
      echo_err "Error: File already exists, please back up and delete: $link_path"
      exit 4
    fi
  else
    # Create link
    echo "Creating link from $link_path to $orig_path..."
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


if [[ $OSTYPE =~ ^darwin ]] then
  echo_msg "Symlinking OS X specific files..."
  create_symlink_to_home 'slate.js'
  create_symlink "$JR_DOTFILES/KeyRemap4MacBook/private.xml" "$HOME/Library/Application Support/KeyRemap4MacBook/private.xml"
  create_symlink "$JR_DOTFILES/Ukelele/US_with_umlauts.keylayout" "$HOME/Library/Keyboard Layouts/US_with_umlauts.keylayout"

  echo_msg "Running OS X configuration script..."
  $JR_DOTFILES/osx.sh
fi


# vim bundles
echo_msg "Installing vim bundles..."
vim -u "$JR_DOTFILES/bundles.vim" +BundleInstall +qall


if [[ "$JR_UPDATE" = "true" ]] then
  echo_msg "Updating..."
  (
    set -e
    cd "$JR_DOTFILES"
    git pull origin master
    git submodule sync
    git submodule update --recursive
    vim -u "$JR_DOTFILES/bundles.vim" '+BundleInstall!'
  )
fi


# Permissions
echo_msg "Updating permissions..."
chmod -- go-rwx $HOME

mkdir -p -- $HOME/.ssh
chmod -R -- go-rwx $HOME/.ssh

mkdir -p -- $HOME/.vim-tmp
chmod -- go-rwx $HOME/.vim-tmp

chmod -R -- go-w $JR_DOTFILES


echo
echo_msg "Done! Installation completed successfully."

# TODO:
# print notes about things to do manually
# don't abort if a symlink exists
