.dotfiles
=========

My dotfiles managed by git.


Installation
------------

1. Install `zsh`, `tmux` and `vim`. Set zsh as default shell (might have to add it to `/etc/shells` first):

        chsh -s `which zsh`

2. Clone the repository to `~/.dotfiles`:

        git clone --recursive https://github.com/JakobR/.dotfiles.git $HOME/.dotfiles

3. Set up symlinks:

        ln -s ~/.dotfiles/zsh/zshrc        ~/.zshrc
        ln -s ~/.dotfiles/zsh/zlogout      ~/.zlogout
        ln -s ~/.dotfiles/zsh/zprofile     ~/.zprofile
        ln -s ~/.dotfiles/tmux.conf        ~/.tmux.conf
        ln -s ~/.dotfiles/vim              ~/.vim
        ln -s ~/.dotfiles/vimrc            ~/.vimrc
        ln -s ~/.dotfiles/gvimrc           ~/.gvimrc
        ln -s ~/.dotfiles/ackrc            ~/.ackrc
        ln -s ~/.dotfiles/gitconfig        ~/.gitconfig
        ln -s ~/.dotfiles/gitignore-global ~/.gitignore-global
        ln -s ~/.dotfiles/ghci             ~/.ghci
        ln -s ~/.dotfiles/inputrc          ~/.inputrc
        ln -s ~/.dotfiles/gemrc            ~/.gemrc

4. Set up vim bundles:

        vim -u ~/.dotfiles/bundles.vim +BundleInstall +qall

5. When connecting over ssh, sshd should accept the `TERM_PROGRAM` variable. Add this line to `sshd_config`:

        AcceptEnv TERM_PROGRAM

6. Start/restart zsh.


### Additional steps on Mac OS X

1. Install some additional software:
    * [PCKeyboardHack](http://pqrs.org/macosx/keyremap4macbook/pckeyboardhack.html.en)
    * [KeyRemap4MacBook](http://pqrs.org/macosx/keyremap4macbook/index.html.en)
    * [Slate](https://github.com/jigish/slate)

2. Additional configuration steps:
    * In `System Preferences / Keyboard / Modifier Keys`, set Caps Lock to "No Action".
    * In PCKeyboardHack, map Caps Lock to "80" (F19).
    * Create symlink for KeyRemap4MacBook's `private.xml` file:
        ```
          ln -sf ~/.dotfiles/KeyRemap4MacBook/private.xml ~/Library/Application\ Support/KeyRemap4MacBook/private.xml
        ```
    * Create symlink for Slate configuration:
        ```
          ln -s ~/.dotfiles/slate.js ~/.slate.js
        ```
    * To enable _US with umlauts_ keyboard layout (created with [Ukelele](http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=ukelele), need to relog before it is available):
        ```
          ln -s ~/.dotfiles/Ukelele/US_with_umlauts.keylayout ~/Library/Keyboard\ Layouts/US_with_umlauts.keylayout
        ```

3. Run `osx.sh`.


Updating
--------

    cd ~/.dotfiles
    git pull
    git submodule update --recursive


TODO
----

* Write a script for the installation process.
  * Creates symlinks (abort if any of the files already exists, and tell the user to delete it first)
* Allow to use a single copy of the dotfiles on a server (read-only, owned by root, maybe auto-updated)
  * Need to move vim's tmp/swap directory somewhere else
    (maybe automatically in ~/.vim-tmp, if ~/.vim/... isn't writable?)
