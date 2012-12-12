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

4. Install [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh):

        $ git clone git clone git://github.com/robbyrussell/oh-my-zsh.git $ZDOTDIR/oh-my-zsh

5. Install the [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting) plugin:

        $ git clone git://github.com/zsh-users/zsh-syntax-highlighting.git $ZDOTDIR/custom/plugins/zsh-syntax-highlighting

6. Set zsh as default shell (likely at a different path on other systems).

        $ chsh -s /usr/local/bin/zsh

7. Start/restart zsh.
