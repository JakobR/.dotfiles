.dotfiles
=========

My dotfiles managed by git.


Installation
------------

1. Install `zsh`, `git`, `tmux` and `vim`. Set zsh as default shell (might have to add it to `/etc/shells` first):

        chsh -s `which zsh`

2. Run the installation script:

        curl https://raw.github.com/JakobR/.dotfiles/master/install.zsh | zsh

3. When connecting over ssh, sshd should accept the `TERM_PROGRAM` variable. Add this line to `sshd_config`:

        AcceptEnv TERM_PROGRAM

4. Start/restart zsh.


### Additional steps on Mac OS X

1. Install some additional software:
    * [PCKeyboardHack](http://pqrs.org/macosx/keyremap4macbook/pckeyboardhack.html.en)
    * [KeyRemap4MacBook](http://pqrs.org/macosx/keyremap4macbook/index.html.en)
    * [Slate](https://github.com/jigish/slate)

2. Additional configuration steps:
    * In `System Preferences / Keyboard / Modifier Keys`, set Caps Lock to "No Action".
        (this is necessary for KeyRemap4MacBook's KeyOverlaidModifier to work correctly.)
    * In PCKeyboardHack, map Caps Lock to "80" (F19).


### Location

Default installation location is `$HOME/.dotfiles`.
To set a different location, set the `JR_DOTFILES` variable before running the install script.

Multiple users can share a single installation, but you need to be careful with permissions.
Also, every user needs a symlink at `$HOME/.dotfiles` pointing to where the dotfiles are installed (normally created by install script).


Updating
--------

Just run the installation script again, with the `--update` parameter:

    $JR_DOTFILES/install.zsh --update
