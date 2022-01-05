###############################
# Windows Subsystem for Linux #
###############################

if [[ "$PWD" == "/mnt/c/Windows" ]] then
    cd ~
fi
if [[ "$PWD" == "/mnt/c/Windows/System32" ]] then
    cd ~
fi

alias explorer="/mnt/c/Windows/explorer.exe"

function o {
    local path
    if (( $# == 0 )) then
        path="$PWD"
    elif (( $# == 1 )) then
        path="$1"
    else
        echo "Error: I don't know what to do with multiple arguments :("
        return 2
    fi
    /mnt/c/Windows/explorer.exe "$(/usr/bin/wslpath -w "$path")"
}
