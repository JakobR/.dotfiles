alias vi="vim"

if [[ "$OSTYPE" = darwin* ]] then
    alias mvim="${HOMEBREW_PREFIX:-/usr/local}/bin/mvim -p"
    # On macOS, use MacVim to edit files, and give focus back to iTerm after closing
    export EDITOR="${HOMEBREW_PREFIX:-/usr/local}/bin/mvim -p --nofork -c ActivateTerminalOnExit"
    # But stay in the console for git commit messages
    export GIT_EDITOR="${HOMEBREW_PREFIX:-/usr/local}/bin/vim +0"
elif (( $+commands[vim] )) then
    export EDITOR="${commands[vim]}"
fi

export VISUAL="${EDITOR}"

if (( $+commands[gvim] )) then
    function m_cmd {
        gvim -g "${@}"
    }
else
    function m_cmd {
        vim "${@}"
    }
fi

# TODO: Functions m2, m3, ... which open in the 2nd, 3rd, ... vim server in the serverlist
# TODO: A function "ms" that lists vim servers, and what project each is associated to.
function m {
    # TODO: When starting a new instance, add the code to switch back to the terminal on exit
    # TODO: Maybe use this server: $(vim --serverlist|tail -n 1)   (latest one started)
    # TODO: Maybe it's possible to use a specific vim server per "project"?
    #       A "project" might be each folder in "~/code", or each folder in "~" if it's not in "~/code" (define a list of "project roots", e.g. ["~/code", "~/Documents/Uni/*/", ..., "~"], to match against)
    #       Other idea:
    #           Use "mvim" to manually open a new vim instance for a project.
    #           "m" just picks the most appropriate open vim server (one with the same project; or if none exists, the first one)
    #           That's probably better than opening new project-specific instances automatically?
    if [[ -z "${@}" ]] then
        if [[ -z "$(m_cmd --serverlist)" ]] then
            # No vim instances running? start a new one
            m_cmd
        else
            # m_cmd --remote-send '<Esc>:tabnew<CR>' --remote-expr 'foreground()'
            m_cmd --remote-send '<Esc>:tabnew<CR>:call foreground()<CR>'
        fi
    else
        if [[ -z "$(m_cmd --serverlist)" ]] then
            # No vim instances running? start a new one
            m_cmd "${@}"
        else
            m_cmd --remote-tab-silent "${@}"
        fi
    fi
}

function mkm {
    if (( $# != 1 )) then
        echo "mkm: please pass exactly one argument"
        return 1
    fi
    local cursor_cmd
    cursor_cmd="$(mk -p vim "$1")"
    if (( $? == 0 )) then
        m "+${cursor_cmd}" "$1"
    fi
}

function mkec {
    if (( $# != 1 )) then
        echo "mkec: please pass exactly one argument"
        return 1
    fi
    local cursor_pos
    cursor_pos="$(mk -p emacs "$1")"
    if (( $? == 0 )) then
        ec "+${cursor_pos}" "$1"
    fi
}

function mke {
    if (( $# != 1 )) then
        echo "mke: please pass exactly one argument"
        return 1
    fi
    local cursor_pos
    cursor_pos="$(mk -p emacs "$1")"
    if (( $? == 0 )) then
        e "+${cursor_pos}" "$1"
    fi
}

# Open today's journal entry.
# Can shift day with ±number as first argument (e.g., "mj -1" for yesterday's entry).
function mj {
    if [[ ! "${1}" =~ ^[+-][0-9]+$ ]] then
        1='+0'
    fi
    local timestamp
    timestamp="$(gdate +%s)"
    timestamp="$(( $timestamp + ( $1 * 60 * 60 * 24 ) ))"
    (
        export DATE="$(gdate --date="@${timestamp}" "+%Y-%m-%d")"
        export TAG="$(gdate --date="@${timestamp}" "+%Y/%m/%d")"
        export WEEKDAY="$(/usr/bin/env LC_TIME=en_GB.UTF-8 gdate --date="@${timestamp}" "+%A")"
        cd "${HOME}/kb/Journal/" && vim "./journal-${DATE}".md
    )
}
