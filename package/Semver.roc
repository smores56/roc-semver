module [
    parse,
    parse_lazy,
    to_str,
    compare,
]

import Types exposing [Semver, Ordering]
import Compare
import Parse
import Error exposing [InvalidSemverError]

## Render a semver to a string.
##
## ```roc
## version_str = "1.2.3-alpha+beta"
## version = parse(version_str)?
##
## expect to_str(version) == version_str
## ```
to_str : Semver -> Str
to_str = |{ major, minor, patch, build, pre_release }|
    pre_release_str =
        if List.is_empty(pre_release) then
            ""
        else
            "-${pre_release |> Str.join_with(".")}"
    build_str =
        if List.is_empty(build) then
            ""
        else
            "+${build |> Str.join_with(".")}"

    "${Num.to_str(major)}.${Num.to_str(minor)}.${Num.to_str(patch)}${pre_release_str}${build_str}"

## Parse a semver from a string.
##
## This fails if there is any leftover text after the string. Use the [parseLazy]
## twin function if you want to get the leftover text after parsing.
##
## ```roc
## version = parse("1.2.3-alpha+beta")
##
## expect version == Ok({ major: 1, minor: 2, patch: 3, pre_release: ["alpha"], build: ["beta"] })
## ```
parse : Str -> Result Semver InvalidSemverError
parse = Parse.semver

## Parse a semver from a string, returning the leftover text after parsing on success.
##
## If you don't need the leftover text, use the [parse] twin function.
##
## ```roc
## version = parse_lazy("1.2.3 abc")
##
## expect version == Ok (
##     { major: 1, minor: 2, patch: 3, pre_release: [], build: [] },
##     " abc",
## )
## ```
parse_lazy : Str -> Result (Semver, Str) InvalidSemverError
parse_lazy = Parse.semver_lazy

## Compare two semvers, useful for sorting.
##
## ```roc
## version1 = { major: 1, minor: 2, patch: 3, pre_release: [], build: [] }
## version2 = { major: 1, minor: 3, patch: 0, pre_release: [], build: [] }
##
## expect compare(version1, version2) == LT
## ```
compare : Semver, Semver -> Ordering
compare = Compare.compare_semvers

expect
    to_str({ major: 1, minor: 2, patch: 3, build: [], pre_release: [] })
    == "1.2.3"

expect
    to_str({ major: 1, minor: 2, patch: 3, build: ["def-", "ghi"], pre_release: ["abc"] })
    == "1.2.3-abc+def-.ghi"
