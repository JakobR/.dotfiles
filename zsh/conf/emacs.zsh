
# This implementation currently only works on macOS
if [[ "$OSTYPE" = darwin* ]] then

    function emacs_is_running {
        $EMACSCLIENT --eval "t" &>/dev/null
    }

    function emacs_ensure_running {
        if ! emacs_is_running; then
            printf "Starting Emacs server"
            /usr/bin/open --background /Applications/Emacs.app
            while ! emacs_is_running; do
                printf "."
                /bin/sleep 0.05
            done
            printf "\n"
        fi
    }

    function e {
        emacs_ensure_running

        # Focus Emacs window
        # Do this first, because if the Emacs GUI was hidden with CMD+H,
        # opening a file with emacsclient will block until the GUI is activated by the user.
        $EMACSCLIENT --eval "(x-focus-frame nil)" >/dev/null

        if [[ -z "${@}" ]] then
            # No args: just focus Emacs for now
            # $EMACSCLIENT --eval "(list-directory \"${PWD}\")"
        else
            $EMACSCLIENT --no-wait "${@}"
        fi
    }

    function eo {
        e -c "~/org/inbox.org"
    }

    function ea {
        e -c --eval "(org-agenda-list)" "(delete-other-windows)"
        # TODO: focus agenda buffer if it is already open
        # See also https://emacs.stackexchange.com/a/3472
    }

    function ec {
        # Only pass -c if emacs is already running,
        # otherwise we end up with an extra empty window.
        if emacs_is_running; then
            e -c "${@}"
        else
            e "${@}"
        fi
    }

fi
