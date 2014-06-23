if type ag >/dev/null 2>/dev/null; then

  # Usually I want to search case-insensitively.
  # Use \ag (prefixed with backslash) for case sensitive search.
  alias ag='ag --ignore-case'

fi
