module [
    star,
    toStr,
    matches,
    VersionReq,
]

import Types exposing [VersionReq]
import Comparator exposing [Comparator]
import Semver exposing [Semver]

## The "*" version requirement, a.k.a. an empty list.
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
##     { comparator: Compatible, major: 2, minor: Ok 5, patch: Err NotSpecified, preRelease: [] },
##     { comparator: LessThan, major: 3, minor: Err NotSpecified, patch: Err NotSpecified, preRelease: [] },
## ]
##
## expect toStr versionReq == "^2.5, <3"
##
## expect toStr [] == "*"
## ```
toStr : VersionReq -> Str
toStr = \versionReq ->
    if List.isEmpty versionReq then
        "*"
    else
        versionReq
        |> List.map Comparator.toStr
        |> Str.joinWith ", "

matches : VersionReq, Semver -> Bool
matches = \req, version ->
    satisfiesAllComparators =
        List.all req \comparator ->
            Comparator.satisfies comparator version

    if !satisfiesAllComparators then
        Bool.false
    else if List.isEmpty version.preRelease then
        Bool.true
    else
        # If a version has a prerelease tag (for example, 1.2.3-alpha.3) then it
        # will only be allowed to satisfy req if at least one comparator with the
        # same major.minor.patch also has a prerelease tag.
        List.any req \comparator ->
            Comparator.preReleaseIsCompatible comparator version

testBaseSemver : Semver
testBaseSemver = { major: 1, minor: 2, patch: 3, preRelease: [], build: [] }

testBaseComparator : Comparator
testBaseComparator = Relation { operator: Exact, version: MajorMinorPatch 1 2 3 }

expect matches star testBaseSemver
expect matches [testBaseComparator] testBaseSemver

expect
    matches [{ testBaseComparator & operator: LessThan }] testBaseSemver
    |> Bool.not

expect
    matches [{ testBaseComparator & operator: LessThanOrEqualTo }] testBaseSemver

expect
    matches [{ testBaseComparator & operator: GreaterThan }] testBaseSemver
    |> Bool.not

expect
    matches [{ testBaseComparator & operator: GreaterThanOrEqualTo }] testBaseSemver

expect
    matches [{ testBaseComparator & operator: GreaterThanOrEqualTo }] testBaseSemver

# TODO: write more tests for this
