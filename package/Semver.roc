module [
    parse,
    parseLazy,
    toStr,
    compare,
    Semver,
]

import Types
import Compare
import Parse
import Error exposing [InvalidSemverError]

## Render a semver to a string.
##
## ```roc
## versionStr = "1.2.3-alpha+beta"
## version = parse versionStr
##
## expect toStr version == versionStr
## ```
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

## Parse a semver from a string.
##
## This fails if there is any leftover text after the string. Use the [parseLazy]
## twin function if you want to get the leftover text after parsing.
##
## ```roc
## version = parse "1.2.3-alpha+beta"
##
## expect version == Ok { major: 1, minor: 2, patch: 3, preRelease: ["alpha"], build: ["beta"] }
## ```
parse : Str -> Result Semver InvalidSemverError
parse = Parse.semver

## Parse a semver from a string, returning the leftover text after parsing on success.
##
## If you don't need the leftover text, use the [parse] twin function.
##
## ```roc
## version = parse "1.2.3 abc"
##
## expect version == Ok {
##     version: { major: 1, minor: 2, patch: 3, preRelease: [], build: [] },
##     rest: " abc",
## }
## ```
parseLazy : Str -> Result { version : Semver, rest : Str } InvalidSemverError
parseLazy = Parse.semverLazy

## Compare two semvers, useful for sorting.
##
## ```roc
## version1 = { major: 1, minor: 2, patch: 3, preRelease: [], build: [] }
## version2 = { major: 1, minor: 3, patch: 0, preRelease: [], build: [] }
##
## expect compare version1 version2 == LT
## ```
compare : Semver, Semver -> Ordering
compare = Compare.compareSemvers

expect
    toStr { major: 1, minor: 2, patch: 3, build: [], preRelease: [] }
    == "1.2.3"

expect
    toStr { major: 1, minor: 2, patch: 3, build: ["def-", "ghi"], preRelease: ["abc"] }
    == "1.2.3-abc+def-.ghi"
