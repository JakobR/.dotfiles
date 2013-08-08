# Set iTerm tab color.
#
# Only if not inside tmux and only when connecting with iTerm
if [[ -z "$TMUX" && "$TERM_PROGRAM" == "iTerm.app" ]] then
  # Select desired color by hostname
  HOSTNAME=$(hostname)
  if [[ $HOSTNAME == "dango"* ]] then
    echo -n -e "\033]6;1;bg;red;brightness;0\a\033]6;1;bg;green;brightness;150\a\033]6;1;bg;blue;brightness;0\a"
  elif  [[ $HOSTNAME == "izumi"* ]] then
    echo -n -e "\033]6;1;bg;red;brightness;100\a\033]6;1;bg;green;brightness;100\a\033]6;1;bg;blue;brightness;255\a"
  elif [[ $HOSTNAME == "ichigo"* ]] then
    echo -n -e "\033]6;1;bg;red;brightness;190\a\033]6;1;bg;green;brightness;50\a\033]6;1;bg;blue;brightness;50\a"
  elif [[ $HOSTNAME == "kappa"* ]] then
    echo -n -e "\033]6;1;bg;red;brightness;230\a\033]6;1;bg;green;brightness;210\a\033]6;1;bg;blue;brightness;0\a"
  fi
fi
