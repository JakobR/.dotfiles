
if type sudo >/dev/null 2>/dev/null; then

  # sudo prompt should mention the user name
  alias sudo='sudo -p "[sudo] enter password for %u: "'

fi
