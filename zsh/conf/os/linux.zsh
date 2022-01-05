#########
# Linux #
#########

JR_OPEN="xdg-open"

alias gdate="date"

# Fixup terminfo
if [[ "$TERM_PROGRAM" == "iTerm.app" ]] then
    TERM=iTerm2.app
fi
