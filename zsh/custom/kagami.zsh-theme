# my zsh theme
# based on the crunch theme
#
# vim: set ft=zsh:

export LSCOLORS="ExfxcxdxBxegedabagacad"

function kagami_git_stash_info () {
  local p_git_stash_count="$(git stash list 2> /dev/null | wc -l | tr -d \ \t)"
  if [[ "${p_git_stash_count}" -ne "0" ]] then
    print " %F{blue}s${p_git_stash_count}"
  fi
}

function prompt_kagami_setup () {
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

  local c_gray="%240F"

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

  # User name
  local p_user="%(!.%196F.%9F)%n"

  # Host name (underlined if connected over ssh)
  local p_host="%105F%m"
  if [[ -n "${SSH_CONNECTION:-}" ]] then
    p_host="%U$p_host%u"
  fi

  # Only show user and host if the user isn't the default user, or if we're connected over ssh
  local p_user_host=""
  if [[ ( ! "$USER" =~ ^[Jj]akob$ ) || ( -n "${SSH_CONNECTION:-}" ) ]] then
      p_user_host="$p_user$c_gray@$p_host$c_gray:"
  fi

  local p_nix_shell=""
  if [[ -n "${IN_NIX_SHELL:-}" ]] then
      p_nix_shell="${c_gray}[%F{green}nix-shell${c_gray}] "
  fi

  # Current working directory
  local p_dir="%220F%~"

  # Branch and status of git repository in current directory
  local p_git="\$(git_prompt_info)"
  local p_git_stash="" #"\$(kagami_git_stash_info)"
  # Configure the oh-my-zsh git_prompt_info helper
  ZSH_THEME_GIT_PROMPT_PREFIX=" $c_gray(%40F"
  ZSH_THEME_GIT_PROMPT_SUFFIX="$c_gray)"
  ZSH_THEME_GIT_PROMPT_CLEAN=" %40F$s_git_check"
  ZSH_THEME_GIT_PROMPT_DIRTY=" %196F$s_git_cross"

  # Show exit code of last command if not zero
  local p_status="%(?..%196F%?! )"

  # Prompt symbol depends on whether we're root or not
  local p_char="%(!.%196F$s_prompt_root.$c_gray$s_prompt_char) "

  # Put it all together!
  PROMPT="%B${p_nix_shell}${c_gray}[$p_user_host$p_dir$c_gray]$p_git$p_git_stash $p_status$p_char%b%f%k"


  # Highlighting
  ZSH_HIGHLIGHT_STYLES[unknown-token]="fg=white"

  local c_command="111" # 033 would be ok too, but 111 is easier to read
  ZSH_HIGHLIGHT_STYLES[alias]="fg=$c_command,bold"
  ZSH_HIGHLIGHT_STYLES[builtin]="fg=$c_command,bold"
  ZSH_HIGHLIGHT_STYLES[function]="fg=$c_command,bold"
  ZSH_HIGHLIGHT_STYLES[command]="fg=$c_command,bold"
  ZSH_HIGHLIGHT_STYLES[precommand]="fg=$c_command,bold,underline"
  ZSH_HIGHLIGHT_STYLES[commandseparator]="fg=black,bg=$c_command"

  ZSH_HIGHLIGHT_STYLES[history-expansion]="fg=white,bg=blue"

  ZSH_HIGHLIGHT_STYLES[path]="fg=yellow,underline"
  ZSH_HIGHLIGHT_STYLES[path_prefix]="underline"
  ZSH_HIGHLIGHT_STYLES[path_approx]="none"
  ZSH_HIGHLIGHT_STYLES[globbing]="fg=black,bg=yellow,underline"

  ZSH_HIGHLIGHT_STYLES[single-quoted-argument]="fg=108,underline"
  ZSH_HIGHLIGHT_STYLES[double-quoted-argument]="fg=108,underline"

  ZSH_HIGHLIGHT_STYLES[reserved-word]="fg=063,bold"

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

prompt_kagami_setup
