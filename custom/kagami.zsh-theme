# my zsh theme
# based on the crunch theme

export LSCOLORS="ExfxcxdxBxegedabagacad"

# Color code explanation at http://lucentbeing.com/blog/that-256-color-thing/
CRUNCH_BRACKET_COLOR="%{[01;38;5;240m%}"
CRUNCH_TIME_COLOR="%{[01;38;5;245m%}"
CRUNCH_USER_COLOR="%{[01;38;5;009m%}"
CRUNCH_HOST_COLOR="%{[01;38;5;105m%}"
CRUNCH_RVM_COLOR="%{[22;38;5;090m%}"
CRUNCH_DIR_COLOR="%{[01;38;5;220m%}"
CRUNCH_GIT_BRANCH_COLOR="%{[01;38;5;040m%}"
CRUNCH_GIT_CLEAN_COLOR="%{[01;38;5;040m%}"
CRUNCH_GIT_DIRTY_COLOR="%{[01;38;5;196m%}"

# These Git variables are used by the oh-my-zsh git_prompt_info helper:
ZSH_THEME_GIT_PROMPT_PREFIX="$CRUNCH_BRACKET_COLOR(%{$reset_color%}$CRUNCH_GIT_BRANCH_COLOR"
ZSH_THEME_GIT_PROMPT_SUFFIX="$CRUNCH_BRACKET_COLOR)%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_CLEAN=" $CRUNCH_GIT_CLEAN_COLOR✓"
ZSH_THEME_GIT_PROMPT_DIRTY=" $CRUNCH_GIT_DIRTY_COLOR✗"

# Our elements:
CRUNCH_TIME_="$CRUNCH_TIME_COLOR%T%{$reset_color%}"
CRUNCH_USER_="$CRUNCH_USER_COLOR%n$CRUNCH_BRACKET_COLOR@$CRUNCH_HOST_COLOR%m$CRUNCH_BRACKET_COLOR:%{$reset_color%}"
if which rvm-prompt &> /dev/null; then
  CRUNCH_RVM_="$CRUNCH_BRACKET_COLOR‹%{$reset_color%}$CRUNCH_RVM_COLOR\${\$(~/.rvm/bin/rvm-prompt i v g)#ruby-}$CRUNCH_BRACKET_COLOR›%{$reset_color%} "
# else
#   if which rbenv &> /dev/null; then
#     CRUNCH_RVM_="$CRUNCH_BRACKET_COLOR"["$CRUNCH_RVM_COLOR\${\$(rbenv version | sed -e 's/ (set.*$//' -e 's/^ruby-//')}$CRUNCH_BRACKET_COLOR"]"%{$reset_color%}"
#   fi
fi
CRUNCH_DIR_="$CRUNCH_DIR_COLOR%~"
CRUNCH_GIT_="\$(git_prompt_info)"
CRUNCH_PROMPT="$CRUNCH_BRACKET_COLOR➭ "

# Put it all together!
PROMPT="$CRUNCH_BRACKET_COLOR"["$CRUNCH_USER_$CRUNCH_DIR_$CRUNCH_BRACKET_COLOR"]" $CRUNCH_GIT_$CRUNCH_PROMPT%{$reset_color%}"
RPROMPT="$CRUNCH_RVM_$CRUNCH_TIME_%{$reset_color%}"

# Highlighting
ZSH_HIGHLIGHT_STYLES[unknown-token]="fg=white"

ZSH_HIGHLIGHT_COMMAND_COLOR="111" # 033 would be ok too, but 111 is easier to read
ZSH_HIGHLIGHT_STYLES[alias]="fg=$ZSH_HIGHLIGHT_COMMAND_COLOR,bold"
ZSH_HIGHLIGHT_STYLES[builtin]="fg=$ZSH_HIGHLIGHT_COMMAND_COLOR,bold"
ZSH_HIGHLIGHT_STYLES[function]="fg=$ZSH_HIGHLIGHT_COMMAND_COLOR,bold"
ZSH_HIGHLIGHT_STYLES[command]="fg=$ZSH_HIGHLIGHT_COMMAND_COLOR,bold"
ZSH_HIGHLIGHT_STYLES[precommand]="fg=$ZSH_HIGHLIGHT_COMMAND_COLOR,bold,underline"
ZSH_HIGHLIGHT_STYLES[commandseparator]="fg=black,bg=$ZSH_HIGHLIGHT_COMMAND_COLOR"

ZSH_HIGHLIGHT_STYLES[path]="fg=yellow,underline"
ZSH_HIGHLIGHT_STYLES[globbing]="fg=black,bg=yellow,underline"

ZSH_HIGHLIGHT_STYLES[single-quoted-argument]="fg=108,underline"
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]="fg=108,underline"

ZSH_HIGHLIGHT_STYLES[reserved-word]="fg=063,bold"

# The default highlight styles:
# : ${ZSH_HIGHLIGHT_STYLES[default]:=none}
# : ${ZSH_HIGHLIGHT_STYLES[unknown-token]:=fg=red,bold}
# : ${ZSH_HIGHLIGHT_STYLES[reserved-word]:=fg=yellow}
# : ${ZSH_HIGHLIGHT_STYLES[hashed-command]:=fg=green}
# : ${ZSH_HIGHLIGHT_STYLES[history-expansion]:=fg=blue}
# : ${ZSH_HIGHLIGHT_STYLES[single-hyphen-option]:=none}
# : ${ZSH_HIGHLIGHT_STYLES[double-hyphen-option]:=none}
# : ${ZSH_HIGHLIGHT_STYLES[back-quoted-argument]:=none}
# : ${ZSH_HIGHLIGHT_STYLES[single-quoted-argument]:=fg=yellow}
# : ${ZSH_HIGHLIGHT_STYLES[double-quoted-argument]:=fg=yellow}
# : ${ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]:=fg=cyan}
# : ${ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]:=fg=cyan}
# : ${ZSH_HIGHLIGHT_STYLES[assign]:=none}
