#!/bin/sh

cd ~/.dotfiles
git pull
git submodule update --recursive
vim -u ~/.dotfiles/bundles.vim '+BundleInstall!'
