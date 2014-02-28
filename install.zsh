#!/usr/bin/env zsh

# This script should be idempotent (running it multiple times should result in the same effect).
# If an error occurs, fix the problem according to the message and run the script again.


# Abort on error
set -e

if [[ -z "$JR_DOTFILES" ]] then
  # Use default location if not set
  export JR_DOTFILES=$HOME/.dotfiles
fi

if [[ ! -d "$JR_DOTFILES" ]] then
  # TODO: Clone automatically (or ask user?)
  echo "Please clone https://github.com/JakobR/.dotfiles.git"
  exit 1
fi

if [[ ! -f "$JR_DOTFILES/.jr_dotfiles" ]] then
  echo "JR_DOTFILES directory (path: '$JR_DOTFILES') does not contain expected data (or is too old?)"
  exit 2
fi


# creates a symlink with additional checks and output
function create_symlink () {
  local orig_path="$1"
  local link_path="$2"

  if [[ ! -e "$orig_path" ]] then
    echo "Error: Original file does not exist: $orig_path"
    exit 3
  fi

  if [[ -e "$link_path" ]] then
    # Link exists. Is it the correct one?
    if [[ (-L "$link_path") && ("$(readlink -n $link_path)" = "$orig_path") ]] then
      echo "Found correct link at $link_path"
    else
      # TODO: Can backup automatically
      echo "Error: File already exists, please back up and delete: $link_path"
      exit 4
    fi
  else
    # Create link
    echo "Creating link from $link_path to $orig_path"
    /bin/ln -is "$orig_path" "$link_path"
  fi
}

# Expects a path relative to $JR_DOTFILES.
function create_symlink_to_home () {
  local file="$1"

  # "${file:t}" is the basename of the path in $file
  create_symlink "$JR_DOTFILES/$file" "$HOME/.${file:t}"
}


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
  create_symlink_to_home 'slate.js'
  create_symlink "$JR_DOTFILES/KeyRemap4MacBook/private.xml" "$HOME/Library/Application Support/KeyRemap4MacBook/private.xml"
fi

# TODO:
# Allow running with curl https://.../ | zsh or so for fast install
# run osx.sh on osx
# also symlink private.xml if on osx
# print notes about things to do manually
