
# Check common rmate locations and alias if found
[[ -s "/opt/rmate" ]]  && alias rmate="/opt/rmate"
[[ -s "$HOME/rmate" ]] && alias rmate="$HOME/rmate"
