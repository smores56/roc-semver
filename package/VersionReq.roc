module [
    star,
    to_str,
    parse,
    parse_lazy,
    matches,
]

import Parse
import Comparator
import Types exposing [VersionReq, Semver]
import Error exposing [InvalidVersionReqError]

## The "*" version requirement, a.k.a. an empty list of comparators.
##
## This will match any semver.
star : VersionReq
star = []

## Render a version requirement to a string.
##
## This will not always look like the source text a version requirement
## was parsed from, as we don't store ignored text, like whitespace.
##
## ```roc
## versionReq = [
##     Relation { operator: Compatible, version: MajorMinor 2 5 },
##     Relation { operator: LessThan, version: Major 3 },
## ]
##
## expect toStr versionReq == "^2.5, <3"
##
## expect toStr [] == "*"
## ```
to_str : VersionReq -> Str
to_str = |version_req|
    if List.is_empty(version_req) then
        "*"
    else
        version_req
        |> List.map(Comparator.to_str)
        |> Str.join_with(", ")

expect to_str([]) == "*"
expect to_str([Wildcard(MajorMinor(1, 3))]) == "1.3.*"
expect
    to_str(
        [
            Relation({ operator: Compatible, version: MajorMinor(2, 5) }),
            Relation({ operator: LessThan, version: Major(3) }),
        ],
    )
    == "^2.5, <3"

## Parse a version requirement from a string.
##
## This fails if there is any leftover text after the string. Use the [parseLazy]
## twin function if you want to get the leftover text after parsing.
##
## ```roc
## versionReq = parse "1.2.*, <1.2.5"
##
## expect versionReq == Ok [
##     Wildcard (MajorMinor 1 2),
##     Relation { operator: LessThan, version: Full { major: 1, minor: 2, patch: 5, preRelease: [] } },
## ]
## ```
parse : Str -> Result VersionReq InvalidVersionReqError
parse = Parse.version_req

## Parse a semver from a string, returning the leftover text after parsing on success.
##
## If you don't need the leftover text, use the [parse] twin function.
##
## ```roc
## versionReq = parse "1.2.*, <1.2.5 ?"
##
## expect versionReq == (
##     Ok [
##         Wildcard (MajorMinor 1 2),
##         Relation { operator: LessThan, version: Full { major: 1, minor: 2, patch: 5, preRelease: [] } },
##     ],
##     " ?",
## )
## ```
parse_lazy : Str -> Result (VersionReq, Str) InvalidVersionReqError
parse_lazy = Parse.version_req_lazy

## Check if a semver matches a version requirement.
##
## The version must match all comparators, and if it has a pre-release,
## at least one of the comparators must have a pre-release _and_ fully
## match the version's segments.
##
## ```roc
## versionReq = [Relation { operator: Exact, version: Major 1 }]
## semver = { major: 1, minor: 3, patch: 5, preRelease: [], build: [] }
##
## expect versionReq |> matches semver
## ```
matches : VersionReq, Semver -> Bool
matches = |req, version|
    satisfies_all_comparators =
        List.all(
            req,
            |comparator|
                comparator |> Comparator.accepts(version),
        )

    if !satisfies_all_comparators then
        Bool.false
    else if List.is_empty(version.pre_release) then
        Bool.true
    else
        # If a version has a prerelease tag (for example, 1.2.3-alpha.3) then it
        # will only be allowed to satisfy req if at least one comparator with the
        # same major.minor.patch also has a prerelease tag.
        List.any(
            req,
            |comparator|
                Comparator.pre_release_is_compatible(comparator, version),
        )

test_semver = { major: 1, minor: 2, patch: 3, pre_release: [], build: [] }
test_comparator = Relation(
    {
        operator: Exact,
        version: Full({ major: 1, minor: 2, patch: 3, pre_release: [] }),
    },
)

expect matches(star, test_semver)
expect matches([test_comparator], test_semver)
