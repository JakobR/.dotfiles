# my zsh theme
# based on the crunch theme
#
# vim: set ft=zsh:

export LSCOLORS="ExfxcxdxBxegedabagacad"

# Color code explanation from http://lucentbeing.com/blog/that-256-color-thing/
# The general structure of a color code is:
#   code       :: ^[[(value)m
#   value      :: (attributes);(foreground);(background)
#   attributes :: attribute;attributes
#   attribute  :: 00|01|03|04|05|07|22|23|24|25|27
#   foreground :: 38;05;color
#   background :: 48;05;color
#   color      :: 000-255
# attributes:
#   Reset	        00
#   Bold	        01
#   Italic	      03
#   Underline	    04
#   Blink	        05
#   Reverse	      07
#   No Bold	      22
#   No Italic	    23
#   No Underline  24
#   No Blink	    25
#   No Reverse	  27
KAGAMI_BRACKET_COLOR="%{[00;01;38;5;240m%}"
KAGAMI_TIME_COLOR="%{[00;38;5;245m%}"
KAGAMI_USER_COLOR="%{[00;01;38;5;009m%}"
KAGAMI_ROOT_COLOR_FG="%{[00;01;38;5;196m%}"
KAGAMI_ROOT_COLOR_BG="%{[00;01;38;5;016;48;5;196m%}"
KAGAMI_HOST_COLOR="%{[00;01;38;5;105m%}"
KAGAMI_RVM_COLOR="%{[00;38;5;090m%}"
KAGAMI_DIR_COLOR="%{[00;01;38;5;220m%}"
KAGAMI_GIT_BRANCH_COLOR="%{[00;01;38;5;040m%}"
KAGAMI_GIT_CLEAN_COLOR="%{[00;01;38;5;040m%}"
KAGAMI_GIT_DIRTY_COLOR="%{[00;01;38;5;196m%}"
KAGAMI_STATUS_COLOR="%{[00;01;38;5;196m%}"
KAGAMI_UNDERLINE="%{[04m%}"
KAGAMI_RESET="%{[00m%}"

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
ZSH_THEME_GIT_PROMPT_PREFIX="$KAGAMI_BRACKET_COLOR($KAGAMI_GIT_BRANCH_COLOR"
ZSH_THEME_GIT_PROMPT_SUFFIX="$KAGAMI_BRACKET_COLOR) "
ZSH_THEME_GIT_PROMPT_CLEAN=" $KAGAMI_GIT_CLEAN_COLOR$KAGAMI_SYMBOL_GIT_CHECK"
ZSH_THEME_GIT_PROMPT_DIRTY=" $KAGAMI_GIT_DIRTY_COLOR$KAGAMI_SYMBOL_GIT_CROSS"

# are we root or just a normal user?
if [[ $UID -eq 0 ]] then
  KAGAMI_PROMPT="$KAGAMI_ROOT_COLOR_FG$KAGAMI_SYMBOL_ROOT "
  KAGAMI_USER_COLOR=$KAGAMI_ROOT_COLOR_BG
  # TODO: Maybe with black text/red background it's even more visible?
else
  KAGAMI_PROMPT="$KAGAMI_BRACKET_COLOR$KAGAMI_SYMBOL_PROMPT "
fi

# underline host name if connected over ssh
if [[ -n $SSH_CLIENT ]] then
  KAGAMI_HOST_COLOR="$KAGAMI_HOST_COLOR$KAGAMI_UNDERLINE"
fi

#KAGAMI_TIME_="$KAGAMI_TIME_COLOR%T"
KAGAMI_USER_="$KAGAMI_USER_COLOR%n$KAGAMI_BRACKET_COLOR@$KAGAMI_HOST_COLOR%m$KAGAMI_BRACKET_COLOR:"
#if which rvm-prompt &> /dev/null; then
  #KAGAMI_RVM_="$KAGAMI_BRACKET_COLOR$KAGAMI_SYMBOL_RVM_LEFT$KAGAMI_RVM_COLOR\${\$(~/.rvm/bin/rvm-prompt i v g)#ruby-}$KAGAMI_BRACKET_COLOR$KAGAMI_SYMBOL_RVM_RIGHT "
#fi
KAGAMI_DIR_="$KAGAMI_DIR_COLOR%~"
KAGAMI_GIT_="\$(git_prompt_info)"
KAGAMI_STATUS_="%(?..${KAGAMI_STATUS_COLOR}%?${KAGAMI_BRACKET_COLOR}! )"

# Put it all together!
PROMPT="$KAGAMI_BRACKET_COLOR"["$KAGAMI_USER_$KAGAMI_DIR_$KAGAMI_BRACKET_COLOR"]" $KAGAMI_GIT_$KAGAMI_STATUS_$KAGAMI_PROMPT$KAGAMI_RESET"
#RPROMPT="$KAGAMI_RVM_$KAGAMI_TIME_$KAGAMI_RESET"

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
