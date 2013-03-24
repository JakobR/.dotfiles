.dotfiles
=========

My dotfiles managed by git.


Installation
------------

1. Install `zsh`, `tmux` and `vim`. Set zsh as default shell (might have to add it to `/etc/shells` first):

        $ chsh -s `which zsh`

2. Clone the repository to `~/.dotfiles`:

        $ git clone git://github.com/JakobR/.dotfiles.git $HOME/.dotfiles

3. Clone the submodules:

        $ cd ~/.dotfiles
        $ git submodule init
        $ git submodule update

4. Set up symlinks:

        $ ln -s ~/.dotfiles/zsh/zshrc ~/.zshrc
        $ ln -s ~/.dotfiles/tmux.conf ~/.tmux.conf
        $ ln -s ~/.dotfiles/vimrc     ~/.vimrc

5. When connecting over ssh, sshd should accept the `TERM_PROGRAM` variable. Add this line to `sshd_config`:

        AcceptEnv TERM_PROGRAM

6. Start/restart zsh.


Updating
--------

    $ cd ~/.dotfiles
    $ git pull origin master
    $ git submodule update


TODO
----

* Write a script for the installation process.
  * Creates symlinks (abort if any of the files already exists, and tell the user to delete it first)
