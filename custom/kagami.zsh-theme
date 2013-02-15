# my zsh theme
# based on the crunch theme

export LSCOLORS="ExfxcxdxBxegedabagacad"

# Color code explanation at http://lucentbeing.com/blog/that-256-color-thing/
KAGAMI_BRACKET_COLOR="%{[01;38;5;240m%}"
KAGAMI_TIME_COLOR="%{[01;38;5;245m%}"
KAGAMI_USER_COLOR="%{[01;38;5;009m%}"
KAGAMI_ROOT_COLOR="%{[01;38;5;196m%}"
KAGAMI_HOST_COLOR="%{[01;38;5;105m%}"
KAGAMI_RVM_COLOR="%{[22;38;5;090m%}"
KAGAMI_DIR_COLOR="%{[01;38;5;220m%}"
KAGAMI_GIT_BRANCH_COLOR="%{[01;38;5;040m%}"
KAGAMI_GIT_CLEAN_COLOR="%{[01;38;5;040m%}"
KAGAMI_GIT_DIRTY_COLOR="%{[01;38;5;196m%}"

# Special symbols used by the prompt
if [[ "$ZSH_THEME_NO_SPECIAL_CHARACTERS" != "true" ]] then
  KAGAMI_SYMBOL_GIT_CHECK='✓'
  KAGAMI_SYMBOL_GIT_CROSS='✗'
  KAGAMI_SYMBOL_RVM_LEFT='‹'
  KAGAMI_SYMBOL_RVM_RIGHT='›'
  KAGAMI_SYMBOL_PROMPT='➭'
else
  KAGAMI_SYMBOL_GIT_CHECK='o'
  KAGAMI_SYMBOL_GIT_CROSS='x'
  KAGAMI_SYMBOL_RVM_LEFT='<'
  KAGAMI_SYMBOL_RVM_RIGHT='>'
  KAGAMI_SYMBOL_PROMPT='>'
fi
KAGAMI_SYMBOL_ROOT='#'

# For the oh-my-zsh git_prompt_info helper:
ZSH_THEME_GIT_PROMPT_PREFIX="$KAGAMI_BRACKET_COLOR(%{$reset_color%}$KAGAMI_GIT_BRANCH_COLOR"
ZSH_THEME_GIT_PROMPT_SUFFIX="$KAGAMI_BRACKET_COLOR)%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_CLEAN=" $KAGAMI_GIT_CLEAN_COLOR$KAGAMI_SYMBOL_GIT_CHECK"
ZSH_THEME_GIT_PROMPT_DIRTY=" $KAGAMI_GIT_DIRTY_COLOR$KAGAMI_SYMBOL_GIT_CROSS"

# are we root or just a normal user?
if [[ $UID -eq 0 ]] then
  KAGAMI_PROMPT="$KAGAMI_ROOT_COLOR$KAGAMI_SYMBOL_ROOT "
  KAGAMI_USER_COLOR=$KAGAMI_ROOT_COLOR
  # TODO: Maybe with black text/red background it's even more visible?
else
  KAGAMI_PROMPT="$KAGAMI_BRACKET_COLOR$KAGAMI_SYMBOL_PROMPT "
fi

KAGAMI_TIME_="$KAGAMI_TIME_COLOR%T%{$reset_color%}"
KAGAMI_USER_="$KAGAMI_USER_COLOR%n$KAGAMI_BRACKET_COLOR@$KAGAMI_HOST_COLOR%m$KAGAMI_BRACKET_COLOR:%{$reset_color%}"
if which rvm-prompt &> /dev/null; then
  KAGAMI_RVM_="$KAGAMI_BRACKET_COLOR$KAGAMI_SYMBOL_RVM_LEFT%{$reset_color%}$KAGAMI_RVM_COLOR\${\$(~/.rvm/bin/rvm-prompt i v g)#ruby-}$KAGAMI_BRACKET_COLOR$KAGAMI_SYMBOL_RVM_RIGHT%{$reset_color%} "
fi
KAGAMI_DIR_="$KAGAMI_DIR_COLOR%~"
KAGAMI_GIT_="\$(git_prompt_info)"

# Put it all together!
PROMPT="$KAGAMI_BRACKET_COLOR"["$KAGAMI_USER_$KAGAMI_DIR_$KAGAMI_BRACKET_COLOR"]" $KAGAMI_GIT_$KAGAMI_PROMPT%{$reset_color%}"
RPROMPT="$KAGAMI_RVM_$KAGAMI_TIME_%{$reset_color%}"

# Highlighting
ZSH_HIGHLIGHT_STYLES[unknown-token]="fg=white"

ZSH_HIGHLIGHT_COMMAND_COLOR="111" # 033 would be ok too, but 111 is easier to read
ZSH_HIGHLIGHT_STYLES[alias]="fg=$ZSH_HIGHLIGHT_COMMAND_COLOR,bold"
ZSH_HIGHLIGHT_STYLES[builtin]="fg=$ZSH_HIGHLIGHT_COMMAND_COLOR,bold"
ZSH_HIGHLIGHT_STYLES[function]="fg=$ZSH_HIGHLIGHT_COMMAND_COLOR,bold"
ZSH_HIGHLIGHT_STYLES[command]="fg=$ZSH_HIGHLIGHT_COMMAND_COLOR,bold"
ZSH_HIGHLIGHT_STYLES[precommand]="fg=$ZSH_HIGHLIGHT_COMMAND_COLOR,bold,underline"
ZSH_HIGHLIGHT_STYLES[commandseparator]="fg=black,bg=$ZSH_HIGHLIGHT_COMMAND_COLOR"

ZSH_HIGHLIGHT_STYLES[history-expansion]="fg=white,bg=blue"

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
