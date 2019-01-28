{ templateSearchDirs =
    [ "${env:JR_DOTFILES as Text}/vim/templates"
    , "${env:JR_DOTFILES as Text}/vim/bundle/vim-template/templates"
    ]

, variableOverrides =
    let Eval = constructors < Const : Text | Command : Text >
    let Const = Eval.Const
    let Command = Eval.Command

    in  [ { name = "MAIL", value = Command "git --no-pager config -z user.email" }
        , { name = "USER", value = Command "git --no-pager config -z user.name" }
        ]
}
