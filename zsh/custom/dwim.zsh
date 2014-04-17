# I need ^U for Cmd+Backspace
bindkey "^\\" dwim
bindkey "^U" backward-kill-line

#    dtr
# => dtr --list
_dwim_add_transform '^dtr *(.*--)?$' \
  'BUFFER="dtr --list"
  _dwim_cursor=$#BUFFER'

#    dtr --list
# => dtr -t
_dwim_add_transform '^dtr --list *$' \
  'BUFFER="dtr -t "
  _dwim_cursor=$#BUFFER'

#    dtr -t 1,2,3
# => dtr -t 1,2,3 --list
_dwim_add_transform '^dtr -t [0-9,-]+ *(--remove-and-delete)?$' \
  '_dwim_sed "s/ *(--remove-and-delete)?$/ --list/"'

#    dtr -t 1,2,3 --list
# => dtr -t 1,2,3 --remove-and-delete
_dwim_add_transform '^dtr -t [0-9,-]+ *--list' \
  '_dwim_sed "s/--list.*/--remove-and-delete/"'

#    dtr -t 1,2,3 --remove-and-delete
# => dtr -t 1,2,3 --
_dwim_add_transform '^dtr -t [0-9,-]+ *--remove-and-delete$' \
  '_dwim_sed "s/ *--remove-and-delete$/ --/"
  _dwim_cursor=$#BUFFER'


#     brew info <formula>
# =>  brew install <formula>
_dwim_add_transform '^brew info ' \
  '_dwim_sed "s/^brew info/brew install/"
  _dwim_cursor=$#BUFFER'
