.dotfiles
=========

My dotfiles managed by git.


Installation
------------

1. Install `zsh`, `git`, `tmux` and `vim`. Set zsh as default shell (might have to add it to `/etc/shells` first):

        chsh -s `which zsh`

2. Run the installation script:

        curl https://raw.githubusercontent.com/JakobR/.dotfiles/master/install.zsh | zsh

3. Start/restart zsh.


### Location

Default installation location is `$HOME/.dotfiles`.
To set a different location, set the `JR_DOTFILES` variable before running the install script.

Multiple users can share a single installation, but you need to be careful with permissions.
Also, every user needs a symlink at `$HOME/.dotfiles` pointing to where the dotfiles are installed (normally created by install script).


Updating
--------

Just run the installation script again, with the `--update` parameter:

    $JR_DOTFILES/install.zsh --update
