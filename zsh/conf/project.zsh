## Utilities to work with project directories
#
# Project source folder: $HOME/projects/<project-name>
# Project build folder: $HOME/.cache/build/<project-name>/<configuration>
#
# Additionally, every folder that contains `.git` or `.zsh_history` will be considered a project.
# Each project has its own zsh history.

# TODO:
# instead of hardcoding ~/projects for projects, use a link .jr_project in the build folder
# that links back to the project.
# could also have a link .jr_build in the project folder.

if (( $+commands[grealpath] )) then
    function jr_realpath {
        grealpath "${@}"
    }
else
    function jr_realpath {
        realpath "${@}"
    }
fi

# Print current directory relative to home folder.
function rpwd {
    jr_realpath --relative-to="$HOME" "$(pwd -r)"
}

# Test whether the argument is a project root folder.
function is_project_root {
    [[ (-e "${1}/.git") || (-e "${1}/.zsh_history") || ("${1:P}" = "${HOME:P}") || ("$1" = "/") ]]
}

# Find current project's root folder.
function find_project_root {
    local dir="${1:-$PWD}"
    dir="${dir:P}"  # get realpath of directory
    local reldir="$(jr_realpath --relative-to="$HOME" "$dir")"
    if [[ "$reldir" =~ '^\.cache/build/([^/]+)($|/)' ]] then
        # If we're inside the project's build folder, return the project's real root folder.
        dir="$HOME/projects/${match[1]}"
    else
        # Otherwise, find the project root by going up recursively
        while ! is_project_root "$dir"; do
            dir="${dir:h}"
        done
    fi
    printf "%s\n" "$dir"
}

# Go to the current project's root folder.
function cdr {
    cd "$(find_project_root)"
}

function set_histfile_dir {
    local dir="$1"
    if [[ "$dir" = "/" ]] then
        dir="$HOME"
    fi
    local project_histfile="${dir}/.zsh_history"
    if [[ (! -w "$project_histfile") && ( (-e "$project_histfile") || (! -w "${project_histfile:h}") ) ]] then
        # project_histfile exists and isn't writable, or
        # project_histfile doesn't exist and the parent directory isn't writable
        # => use the default location instead
        project_histfile="${HOME}/.zsh_history"
    fi
    # Switch to project histfile.
    # We follow symbolic links. This makes it possible to link the histories of
    # different folders. If we didn't follow the link, zsh would replace it
    # with a regular file on the next write.
    local real_project_histfile="${project_histfile:P}"
    if [[ ${HISTFILE:P} != "${real_project_histfile}" ]] then
        fc -Pp "${real_project_histfile}"
        echo "New HISTFILE: $HISTFILE"
    fi
}

function set_project_histfile {
    set_histfile_dir "$(find_project_root)"
}

function project_build_root {
    local project_root="$(find_project_root)"
    local project_name="${project_root:t}"
    local dir="${HOME}/.cache/build/${project_name}"
    printf "%s\n" "$dir"
}

# Usage:
#   cdb [CONFIG]
# Go to the current project's build directory.
# CONFIG: the build configuration, defaults to "debug".
function cdb {
    local relpwd="$(rpwd)"
    if [[ ( "${relpwd}" = ".cache/build/"** ) || ( "${relpwd}" = "projects/"** ) ]] then
        local build_cfg="${1:-debug}"
        local build_root="$(project_build_root)"
        local build_dir="${build_root}/${build_cfg}"
        if [[ ! -e "${build_dir}" ]] then
            print -u 2 "$0: creating new build directory: ${build_dir}"
            if [[ ! -e "${build_root}" ]] then
                local project_root="$(find_project_root)"
                /bin/mkdir -p "${build_root}"
                /bin/ln -s "${project_root}/.zsh_history" "${build_root}/.zsh_history"
            fi
            /bin/mkdir -p "${build_dir}"
        fi
        cd "${build_dir}"
    else
        print -u 2 "$0: only usable inside project directories, i.e., ~/projects/<name>"
        return 1
    fi
}
