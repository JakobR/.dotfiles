
# Set iTerm tab color.
#
# Only if not inside tmux and only when connecting with iTerm
if [[ -z "$TMUX" && "$TERM_PROGRAM" == "iTerm.app" ]] then
  # Select desired color by hostname
  if [[ `hostname` == "dango"* ]] then
    echo -n -e "\033]6;1;bg;red;brightness;0\a\033]6;1;bg;green;brightness;150\a\033]6;1;bg;blue;brightness;0\a"
  elif  [[ `hostname` == "izumi"* ]] then
    echo -n -e "\033]6;1;bg;red;brightness;100\a\033]6;1;bg;green;brightness;100\a\033]6;1;bg;blue;brightness;255\a"
  elif [[ `hostname` == "ichigo"* ]] then
    echo -n -e "\033]6;1;bg;red;brightness;190\a\033]6;1;bg;green;brightness;50\a\033]6;1;bg;blue;brightness;50\a"
  fi
fi

# TODO: Reset color when disconnecting ssh.
# How is this possible? Maybe overriding 'exit'?
# Could also set the color not only on connect, but every time the prompt is drawn.
