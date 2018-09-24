function AdjustHaskellStackConfig()
    let l:path = expand('%:p')
    let l:resolver = GetHaskellResolver()
    python3 << EOP
from pathlib import Path
import vim
filepath = Path(vim.eval('l:path'))
is_project = None
for p in filepath.parents:
    if list(p.glob('?*.cabal')):
        # parent "p" contains a cabal file, so we assume it is the project root
        is_project = True
        break
else:
    # We exhausted the parents without finding a cabal file, so we assume this is a standalone script
    is_project = False
vim.vars['adjust_haskell_stack_config_is_project'] = is_project
EOP
    if g:adjust_haskell_stack_config_is_project
        " In a project => delete first paragraph of the template (the stack config)
        normal! mqgg"_dap`q
    endif
endfunction

autocmd BufNewFile *.hs silent call AdjustHaskellStackConfig()
