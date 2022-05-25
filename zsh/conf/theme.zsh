# my zsh theme
# based on the crunch theme
#
# vim: set ft=zsh:

autoload -U colors && colors

# Perform parameter expansion, command substitution and arithmetic expansion in prompts
setopt prompt_subst

# See https://apple.stackexchange.com/a/282189
export LSCOLORS="ExfxcxdxBxegedabagacad"
# export LSCOLORS="Gxfxcxdxbxegedabagacad"  TODO: check this

function jr_git_prompt_info {
    if [[ ( -v WSL_DISTRO_NAME ) && ( "${PWD:A}" =~ "^/mnt/(.)(/|$)" ) ]] then
        print " [${match[1]:u}:\\\\]"
        return
    fi
    git_prompt_info
}

function jr_git_stash_info {
    local p_git_stash_count="$(git stash list 2> /dev/null | wc -l | tr -d \ \t)"
    if [[ "${p_git_stash_count}" -ne "0" ]] then
        print " %F{blue}s${p_git_stash_count}"
    fi
}

function jr_prompt_setup {
    # Prompt expansion:
    # see http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
    #
    # %n      user name
    # %m      hostname
    # %~      current working directory
    #
    # %B %b   start/stop boldface mode
    # %U %u   start/stop underline mode
    # %F{123} use foreground color 123 -- can also be written as %123F
    # %K{123} use background color 123
    # %f %k   reset foreground/background color

    # Set default colors for dark background
    local c_user_root="%196F"
    local c_user="%9F"
    local c_host="%105F"
    local c_arch="%196F"
    local c_nix_shell="%F{green}"
    local c_isengard="%196F"
    local c_status_err="%196F"
    local c_dir="%220F"
    local c_git_branch="%40F"
    local c_git_clean="%40F"
    local c_git_dirty="%196F"
    local c_gray="%240F"
    local c_hl_command="111"
    local c_hl_quote="114"
    local c_hl_keyword="063"

    case "${ZSH_THEME_BACKGROUND:-dark}" in
        (light)
            local c_dir="%178F"
            local c_git_branch="%34F"
            local c_git_clean="%34F"
            local c_hl_command="033"
            local c_hl_quote="029"
            ;;
    esac

    # Special symbols used by the prompt
    if [[ "${ZSH_THEME_NO_SPECIAL_CHARACTERS:-false}" != "true" ]] then
        local s_git_check='✓'
        local s_git_cross='✗'
        local s_prompt_char='➭'
    else
        local s_git_check='o'
        local s_git_cross='x'
        local s_prompt_char='>'
    fi
    local s_prompt_root='#'

    # Show active CPU architecture, if it differs from the native one.
    local p_arch=""
    if [[ "$OSTYPE" =~ ^darwin ]] then
        if [[ $(sysctl -n machdep.cpu.brand_string) =~ ^Apple ]] then
            if [[ "$(arch)" != "arm64" ]] then
                p_arch="${c_gray}[${c_arch}$(arch)${c_gray}] "
            fi
        fi
    fi

    # Only show user and host if the user isn't the default user, or if we're connected over ssh
    local p_user_host=""
    if [[ ( ! "$USER" =~ ^[Jj]akob$ ) || ( -n "${SSH_CONNECTION:-}" ) ]] then
        # User name
        local p_user="%(!.${c_user_root}.${c_user})%n"

        # Host name (underlined if connected over ssh)
        local p_host="${c_host}%m"
        if [[ -n "${SSH_CONNECTION:-}" ]] then
            p_host="%U$p_host%u"
        fi

        p_user_host="$p_user$c_gray@$p_host$c_gray:"
    fi

    local p_nix_shell=""
    if [[ -n "${IN_NIX_SHELL:-}" ]] then
        p_nix_shell="${c_gray}[${c_nix_shell}nix-shell${c_gray}] "
    fi

    local p_isengard=""
    if [[ -n "${AWS_PROFILE}" ]] then
        p_isengard="${c_gray}[${c_isengard}${AWS_PROFILE}${c_gray}] "
    fi

    # Current working directory
    local p_dir="${c_dir}%~"

    # Branch and status of git repository in current directory
    # TODO: show git user name! if it's not my main identity. function: git_current_user_name
    local p_git="\$(jr_git_prompt_info)"
    local p_git_stash="" #"\$(jr_git_stash_info)"
    # Configure the oh-my-zsh git_prompt_info helper
    ZSH_THEME_GIT_PROMPT_PREFIX=" ${c_gray}(${c_git_branch}"
    ZSH_THEME_GIT_PROMPT_SUFFIX="${c_gray})"
    ZSH_THEME_GIT_PROMPT_CLEAN=" ${c_git_clean}${s_git_check}"
    ZSH_THEME_GIT_PROMPT_DIRTY=" ${c_git_dirty}${s_git_cross}"

    # Show exit code of last command if not zero
    local p_status="%(?..${c_status_err}%?! )"

    # Prompt symbol depends on whether we're root or not
    local p_prompt="%(!.${c_user_root}${s_prompt_root}${c_gray}.${c_gray}${s_prompt_char}) "

    # Put it all together!
    PROMPT="%B${p_arch}${p_nix_shell}${p_isengard}${c_gray}[$p_user_host$p_dir$c_gray]$p_git$p_git_stash $p_status$p_prompt%b%f%k"

    RPROMPT="${c_gray}%*"

    # Highlighting
    ZSH_HIGHLIGHT_STYLES[unknown-token]="fg=gray"

    ZSH_HIGHLIGHT_STYLES[alias]="fg=$c_hl_command,bold"
    ZSH_HIGHLIGHT_STYLES[builtin]="fg=$c_hl_command,bold"
    ZSH_HIGHLIGHT_STYLES[function]="fg=$c_hl_command,bold"
    ZSH_HIGHLIGHT_STYLES[command]="fg=$c_hl_command,bold"
    ZSH_HIGHLIGHT_STYLES[precommand]="fg=$c_hl_command,bold,underline"
    ZSH_HIGHLIGHT_STYLES[commandseparator]="fg=black,bg=$c_hl_command"

    ZSH_HIGHLIGHT_STYLES[history-expansion]="fg=white,bg=blue"

    ZSH_HIGHLIGHT_STYLES[path]="fg=yellow,underline"
    ZSH_HIGHLIGHT_STYLES[path_prefix]="underline"
    ZSH_HIGHLIGHT_STYLES[path_approx]="none"
    ZSH_HIGHLIGHT_STYLES[globbing]="fg=black,bg=yellow,underline"

    ZSH_HIGHLIGHT_STYLES[single-quoted-argument]="fg=${c_hl_quote},underline"
    ZSH_HIGHLIGHT_STYLES[double-quoted-argument]="fg=${c_hl_quote},underline"

    ZSH_HIGHLIGHT_STYLES[reserved-word]="fg=${c_hl_keyword},bold"

    # The default highlight styles:
    # : ${ZSH_HIGHLIGHT_STYLES[default]:=none}
    # : ${ZSH_HIGHLIGHT_STYLES[unknown-token]:=fg=red,bold}
    # : ${ZSH_HIGHLIGHT_STYLES[reserved-word]:=fg=yellow}
    # : ${ZSH_HIGHLIGHT_STYLES[alias]:=fg=green}
    # : ${ZSH_HIGHLIGHT_STYLES[builtin]:=fg=green}
    # : ${ZSH_HIGHLIGHT_STYLES[function]:=fg=green}
    # : ${ZSH_HIGHLIGHT_STYLES[command]:=fg=green}
    # : ${ZSH_HIGHLIGHT_STYLES[precommand]:=fg=green,underline}
    # : ${ZSH_HIGHLIGHT_STYLES[commandseparator]:=none}
    # : ${ZSH_HIGHLIGHT_STYLES[hashed-command]:=fg=green}
    # : ${ZSH_HIGHLIGHT_STYLES[path]:=underline}
    # : ${ZSH_HIGHLIGHT_STYLES[path_prefix]:=underline}
    # : ${ZSH_HIGHLIGHT_STYLES[path_approx]:=fg=yellow,underline}
    # : ${ZSH_HIGHLIGHT_STYLES[globbing]:=fg=blue}
    # : ${ZSH_HIGHLIGHT_STYLES[history-expansion]:=fg=blue}
    # : ${ZSH_HIGHLIGHT_STYLES[single-hyphen-option]:=none}
    # : ${ZSH_HIGHLIGHT_STYLES[double-hyphen-option]:=none}
    # : ${ZSH_HIGHLIGHT_STYLES[back-quoted-argument]:=none}
    # : ${ZSH_HIGHLIGHT_STYLES[single-quoted-argument]:=fg=yellow}
    # : ${ZSH_HIGHLIGHT_STYLES[double-quoted-argument]:=fg=yellow}
    # : ${ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]:=fg=cyan}
    # : ${ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]:=fg=cyan}
    # : ${ZSH_HIGHLIGHT_STYLES[assign]:=none}
}

jr_prompt_setup
