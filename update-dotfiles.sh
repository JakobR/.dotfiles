#!/bin/sh

cd $JR_DOTFILES
git pull origin master
git submodule sync
git submodule update --recursive
vim -u $JR_DOTFILES/bundles.vim '+BundleInstall!'
