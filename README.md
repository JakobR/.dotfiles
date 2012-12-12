.zsh
====

My zsh configuration.


Installation
------------

1. Set the `$ZDOTDIR` variable. Add this to `/etc/zprofile`:

        # configuration in ~/.zsh
        export ZDOTDIR=$HOME/.zsh

2. Set the `$ZDOTDIR` variable in the current shell session (for the following commands).

3. Clone the repository:

        $ git clone https://github.com/JakobR/.zsh.git $ZDOTDIR

4. Choose one of the zshrc files and link to it:

        $ cd $ZDOTDIR
        $ ln -s zshrc-kagami .zshrc

5. Install [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh):

        $ git clone git clone git://github.com/robbyrussell/oh-my-zsh.git $ZDOTDIR/oh-my-zsh

6. Install the [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting) plugin:

        $ git clone git://github.com/zsh-users/zsh-syntax-highlighting.git $ZDOTDIR/custom/plugins/zsh-syntax-highlighting

7. Set zsh as default shell (likely at a different path on other systems).

        $ chsh -s /usr/local/bin/zsh

8. Start/restart zsh.

### TODO

Write a script for the installation process.
* Should check at the beginning, if $ZDOTDIR is set correctly (or rather, confirm the setting with the user.)
* Should ask which of the zshrc's to link to, maybe make a guess based on `uname -s` output.
