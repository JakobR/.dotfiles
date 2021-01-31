{ templateSearchDirs =
    [ "${env:JR_DOTFILES as Text}/vim/templates/10-pre"
    , "${env:JR_DOTFILES as Text}/vim/templates"
    , "${env:JR_DOTFILES as Text}/vim/bundle/vim-template/templates"
    , "${env:JR_DOTFILES as Text}/vim/templates/99-fallback"
    ]

, variableOverrides =
    let Eval = < Const : Text | Command : Text >
    let Const = Eval.Const
    let Command = Eval.Command

    in  [ { name = "MAIL", value = Command "git --no-pager config user.email" }
        , { name = "USER", value = Command "git --no-pager config user.name" }
        , { name = "LICENSE", value = Command "${env:JR_DOTFILES as Text}/mk/license.zsh" }
        , { name = "NEURONDATE", value = Command "gdate +%Y-%m-%dT%H:%M" }
        , { name = "WEEKDAY", value = Command "/usr/bin/env LC_TIME=en_GB.UTF-8 gdate +%A" }
        , { name = "ENVWEEKDAY", value = Const (env:WEEKDAY as Text ? "?") }
        , { name = "ENVDATE", value = Const (env:DATE as Text ? "????-??-??") }
        , { name = "ENVTAG", value = Const (env:TAG as Text ? "???") }
        -- TODO: need newer dhall version for Text/replace
        -- , { name = "JOURNALTAG", value = Const (
        --         let isodate = env:DATE as Text ? "???"
        --         in Text/replace "-" "/" isodate
        --             )
        --   }
        ]
}
