# my zsh theme
# based on the crunch theme

# Color code explanation at http://lucentbeing.com/blog/that-256-color-thing/
CRUNCH_BRACKET_COLOR="%{[01;38;5;240m%}"
CRUNCH_TIME_COLOR="%{[01;38;5;245m%}"
CRUNCH_USER_COLOR="%{[01;38;5;009m%}"
CRUNCH_HOST_COLOR="%{[01;38;5;063m%}"
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
CRUNCH_USER_="$CRUNCH_USER_COLOR%n$CRUNCH_BRACKET_COLOR@$CRUNCH_HOST_COLOR%m$CRUNCH_BRACKET_COLOR %{$reset_color%}"
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
