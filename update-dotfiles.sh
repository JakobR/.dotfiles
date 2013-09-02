#!/bin/sh

cd $JR_DOTFILES
git pull
git submodule update --recursive
vim -u $JR_DOTFILES/bundles.vim '+BundleInstall!'
