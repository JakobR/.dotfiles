# I need ^U for Cmd+Backspace
bindkey "^\\" dwim
bindkey "^U" backward-kill-line

#    dtr
# => dtr --list
_dwim_add_transform '^dtr *(.*--remove-and-delete)?$' \
  'BUFFER="dtr --list"'

#    dtr --list
# => dtr -t
_dwim_add_transform '^dtr --list *$' \
  'BUFFER="dtr -t "
  _dwim_cursor=$#BUFFER'

#    dtr -t 1,2,3
# => dtr -t 1,2,3 --info | grep Name
_dwim_add_transform '^dtr -t [0-9,-]+ *(--remove-and-delete)?$' \
  '_dwim_sed "s/ *(--remove-and-delete)?$/ --info | grep Name/"'

#    dtr -t 1,2,3 --info | grep Name
# => dtr -t 1,2,3 --remove-and-delete
_dwim_add_transform '^dtr -t [0-9,-]+ *--info *\| *grep N?ame' \
  '_dwim_sed "s/--info.*/--remove-and-delete/"'
