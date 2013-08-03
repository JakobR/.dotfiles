#!/bin/sh

cd ~/.dotfiles
git pull origin master
git submodule update --recursive
vim -u ~/.dotfiles/bundles.vim '+BundleInstall!'
