#!/usr/bin/env zsh

# Abort on errors
set -euo pipefail

function find_license_file {
    dir="$1"
    if [[ -f "${dir}/LICENSE" ]] then
        echo "${dir}/LICENSE"
    elif [[ -f "${dir}/COPYING" ]] then
        echo "${dir}/COPYING"
    else
        if [[ "$dir" != "/" ]] then
            find_license_file "${dir:h}"
        fi
    fi
}

lic_file="$(find_license_file "$PWD")"

lic=""
if [[ ! -z "$lic_file" ]] then
    lic=$(licensee detect --json "$lic_file" | jq -r '.licenses[].spdx_id')
else
    lic="MIT"
fi

if [[ "$lic" == "GPL-3.0" ]] then
    lic="${lic}-or-later"
fi

if [[ -z "$lic" ]] then
    exit 1
fi

echo "$lic"
