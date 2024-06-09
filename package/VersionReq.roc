module [
    star,
    matches,
    VersionReq,
    Comparator,
    ComparisonOperator,
]

import Semver exposing [Semver]
import Compare exposing [comparePreReleases]

VersionReq : List Comparator

Comparator : {
    operator : ComparisonOperator,
    major : U64,
    minor : Result U64 [NotSpecified],
    patch : Result U64 [NotSpecified],
    preRelease : List Str,
}

ComparisonOperator : [
    Exact,
    GreaterThan,
    GreaterThanOrEqualTo,
    LessThan,
    LessThanOrEqualTo,
    PatchUpdates,
    Compatible,
    Wildcard,
]

star : VersionReq
star = []

matches : VersionReq, Semver -> Bool
matches = \req, version ->
    satisfiesAllComparators =
        List.all req \comparator ->
            comparatorMatches comparator version

    if !satisfiesAllComparators then
        Bool.false
    else if List.isEmpty version.preRelease then
        Bool.true
    else
        # If a version has a prerelease tag (for example, 1.2.3-alpha.3) then it
        # will only be allowed to satisfy req if at least one comparator with the
        # same major.minor.patch also has a prerelease tag.
        List.any req \comparator ->
            preReleaseIsCompatible comparator version

comparatorMatches : Comparator, Semver -> Bool
comparatorMatches = \comp, version ->
    when comp.operator is
        Exact | Wildcard -> matchesExact comp version
        GreaterThan -> matchesGreater comp version
        GreaterThanOrEqualTo -> matchesExact comp version || matchesGreater comp version
        LessThan -> matchesLess comp version
        LessThanOrEqualTo -> matchesExact comp version || matchesLess comp version
        PatchUpdates -> matchesPatchUpdates comp version
        Compatible -> matchesCompatible comp version

matchesExact : Comparator, Semver -> Bool
matchesExact = \comp, { major, minor, patch, preRelease } ->
    majorMatches = comp.major == major
    minorMatches =
        comp.minor
        |> Result.map \m -> m == minor
        |> Result.withDefault Bool.true
    patchMatches =
        comp.patch
        |> Result.map \p -> p == patch
        |> Result.withDefault Bool.true
    preReleaseMatches = comp.preRelease == preRelease

    majorMatches && minorMatches && patchMatches && preReleaseMatches

matchesGreater : Comparator, Semver -> Bool
matchesGreater = \comp, { major, minor, patch, preRelease } ->
    when comp is
        { major: compMajor } if compMajor != major -> major > compMajor
        { minor: Err NotSpecified } -> Bool.false
        { minor: Ok compMinor } if compMinor != minor -> minor > compMinor
        _otherwise ->
            when comp.patch is
                Err NotSpecified -> Bool.false
                Ok compPatch ->
                    if compPatch != patch then
                        patch > compPatch
                    else
                        comparePreReleases preRelease comp.preRelease == GT

matchesLess : Comparator, Semver -> Bool
matchesLess = \comp, { major, minor, patch, preRelease } ->
    when comp is
        { major: compMajor } if compMajor != major -> major < compMajor
        { minor: Err NotSpecified } -> Bool.false
        { minor: Ok compMinor } if compMinor != minor -> minor < compMinor
        _otherwise ->
            when comp.patch is
                Err NotSpecified -> Bool.false
                Ok compPatch ->
                    if compPatch != patch then
                        patch < compPatch
                    else
                        comparePreReleases preRelease comp.preRelease == LT

matchesPatchUpdates : Comparator, Semver -> Bool
matchesPatchUpdates = \comp, { major, minor, patch, preRelease } ->
    minorDoesntMatch =
        comp.minor
        |> Result.map \m -> m != minor
        |> Result.withDefault Bool.false

    if major != comp.major then
        Bool.false
    else if minorDoesntMatch then
        Bool.false
    else
        when comp.patch is
            Ok compPatch if compPatch != patch -> patch > compPatch
            _otherwise -> comparePreReleases preRelease comp.preRelease != LT

matchesCompatible : Comparator, Semver -> Bool
matchesCompatible = \comp, { major, minor, patch, preRelease } ->
    if comp.major != major then
        Bool.false
    else
        when (comp.minor, comp.patch) is
            (Err NotSpecified, Err NotSpecified) | (Err NotSpecified, Ok _compPatch) ->
                Bool.true

            (Ok compMinor, Err NotSpecified) ->
                if comp.major > 0 then
                    minor >= compMinor
                else
                    minor == compMinor

            (Ok compMinor, Ok compPatch) ->
                if comp.major > 0 && minor != compMinor then
                    minor > compMinor
                else if comp.major > 0 && patch != compPatch then
                    patch > compPatch
                else if compMinor > 0 && minor != compMinor then
                    Bool.false
                else if compMinor > 0 && patch != compPatch then
                    patch > compPatch
                else if minor != compMinor || patch != compPatch then
                    Bool.false
                else
                    comparePreReleases preRelease comp.preRelease != LT

preReleaseIsCompatible : Comparator, Semver -> Bool
preReleaseIsCompatible = \comp, { major, minor, patch } ->
    majorMatches = comp.major == major
    minorMatches = comp.minor == Ok minor
    patchMatches = comp.patch == Ok patch
    hasPreRelease = !(List.isEmpty comp.preRelease)

    majorMatches && minorMatches && patchMatches && hasPreRelease

testBaseSemver : Semver
testBaseSemver = { major: 1, minor: 2, patch: 3, preRelease: [], build: [] }

testBaseComparator : Comparator
testBaseComparator = { operator: Exact, major: 1, minor: Ok 2, patch: Ok 3, preRelease: [] }

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
