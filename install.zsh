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

# Files to link from home directory.
# This array contains paths relative to $JR_DOTFILES.
# Every entry will be linked to $HOME/.`basename $file`
files=(
  zsh/zshrc
  zsh/zlogout
  zsh/zprofile
  tmux.conf
  vim
  vimrc
  gvimrc
  ackrc
  gitconfig
  gitignore-global
  ghci
  inputrc
  gemrc
)

for file in $files
do
  link_loc="$HOME/.$(basename $file)"
  orig_loc="$JR_DOTFILES/$file"

  if [[ ! -e "$orig_loc" ]] then
    echo "Error: Original file does not exist: $orig_loc"
    exit 3
  fi

  if [[ -e "$link_loc" ]] then
    # Link exists. Is it the correct one?
    if [[ (-L "$link_loc") && ("$(readlink -n $link_loc)" = "$orig_loc") ]] then
      echo "Found correct link at $link_loc"
    else
      # TODO: Can backup automatically
      echo "Error: File already exists, please back up and delete: $link_loc"
      exit 4
    fi
  else
    # Create link
    echo "Creating link from $link_loc to $orig_loc"
    /bin/ln -is "$orig_loc" "$link_loc"
  fi

done

## previous complicated version with associative array:
#
# typeset -A files
# files=(
#   .zshrc              zsh/zshrc
#   .zlogout            zsh/zlogout
#   .zprofile           zsh/zprofile
#   .tmux.conf          -
#   .vim                -
#   .vimrc              -
#   .gvimrc             -
#   .ackrc              -
#   .gitconfig          -
#   .gitignore-global   -
#   .ghci               -
#   .inputrc            -
#   .gemrc              -
# )

# for k in ${(k)files}
# do
#   v="${files[$k]}"

#   link_dst="$HOME/$k"

#   if [[ "$v" = "-" ]] then
#     # A dash means default location -> just remove the dot
#     link_src="${k[2,$#k]}"
#   else
#     link_src="$v"
#   fi
#   link_src="$JR_DOTFILES/$link_src"

#   echo $link_src '->' $link_dst
# done


platform=`uname -s`

# TODO:
# Allow running with curl https://.../ | zsh or so for fast install
# run osx.sh on osx
# also symlink private.xml if on osx
# print notes about things to do manually
