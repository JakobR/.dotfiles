{ templateSearchDirs =
    [ "${env:JR_DOTFILES as Text}/vim/templates/10-pre"
    , "${env:JR_DOTFILES as Text}/vim/templates"
    , "${env:JR_DOTFILES as Text}/vim/bundle/vim-template/templates"
    , "${env:JR_DOTFILES as Text}/vim/templates/99-fallback"
    ]

, variableOverrides =
    let Eval = constructors < Const : Text | Command : Text >
    let Const = Eval.Const
    let Command = Eval.Command

    in  [ { name = "MAIL", value = Command "git --no-pager config -z user.email | tr -d \"\\000\"" }
        , { name = "LICENSE", value = Const "MIT" }
        ]
}
