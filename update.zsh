#!/usr/bin/env zsh

# This script should be idempotent (running it multiple times should result in the same effect).
# If an error occurs, fix the problem according to the message and run the script again.


# Abort on error
set -e

if [[ -z "$JR_DOTFILES" ]] then
  echo "\$JR_DOTFILES variable does not exist. Please install correctly and try again."
  exit 1
fi

if [[ ! -d "$JR_DOTFILES" ]] then
  echo "'$JR_DOTFILES' directory does not exist. Please install correctly and try again."
  exit 2
fi

if [[ ! -f "$JR_DOTFILES/.jr_dotfiles" ]] then
  echo "JR_DOTFILES directory (path: '$JR_DOTFILES') does not contain expected data. Please install correctly and try again."
  exit 3
fi

cd $JR_DOTFILES

# if [[ ! -f ".jr_dotfiles" ]]
# then
#   echo 'Error: Check if $JR_DOTFILES is set correctly and .jr_dotfiles exists in that directory.' >&2
#   exit 2
# fi

git pull origin master
git submodule sync
git submodule update --recursive
vim -u $JR_DOTFILES/bundles.vim '+BundleInstall!'

# TODO:
# check if new files to symlink exist, prompt to user if they do (maybe install too if possible)
#
# should maybe add the functionality of this script to install.zsh:
# -> install.zsh installs dotfiles (and repairs current installation if already installed)
# -> install.zsh --update updates the installation
