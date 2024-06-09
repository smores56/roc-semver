module [
    toStr,
    Semver,
]

# TODO: add Sort implementation once that's merged: https://github.com/roc-lang/roc/pull/6615

Semver : {
    major : U64,
    minor : U64,
    patch : U64,
    build : List Str,
    preRelease : List Str,
}

toStr : Semver -> Str
toStr = \{ major, minor, patch, build, preRelease } ->
    preReleaseStr =
        if List.isEmpty preRelease then
            ""
        else
            "-$(preRelease |> Str.joinWith ".")"
    buildStr =
        if List.isEmpty build then
            ""
        else
            "+$(build |> Str.joinWith ".")"

    "$(Num.toStr major).$(Num.toStr minor).$(Num.toStr patch)$(preReleaseStr)$(buildStr)"

expect
    toStr { major: 1, minor: 2, patch: 3, build: [], preRelease: [] }
    == "1.2.3"

expect
    toStr { major: 1, minor: 2, patch: 3, build: ["def-", "ghi"], preRelease: ["abc"] }
    == "1.2.3-abc+def-.ghi"
