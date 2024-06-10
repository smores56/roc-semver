module [
    toStr,
    satisfies,
    preReleaseIsCompatible,
    Comparator,
    ComparatorOperator,
]

import Semver exposing [Semver]
import Compare exposing [comparePreReleases]
import Types exposing [Comparator, RelationComparatorOperator]

## Render a comparator to a string.
toStr : Comparator -> Str
toStr = \comp ->
    when comp is
        Wildcard version ->
            when version is
                Full -> "*"
                Major major -> "$(Num.toStr major).*"
                MajorMinor major minor -> "$(Num.toStr major).$(Num.toStr minor).*"

        Relation { version, operator } ->
            versionStr =
                when version is
                    Major major ->
                        "$(Num.toStr major)"

                    MajorMinor major minor ->
                        "$(Num.toStr major).$(Num.toStr minor)"

                    MajorMinorPatch major minor patch ->
                        "$(Num.toStr major).$(Num.toStr minor).$(Num.toStr patch)"

                    MajorMinorPatchPreRelease major minor patch preRelease ->
                        "$(Num.toStr major).$(Num.toStr minor).$(Num.toStr patch)-$(Str.joinWith preRelease ".")"

            "$(operatorToStr operator)$(versionStr)"

operatorToStr : RelationComparatorOperator -> Str
operatorToStr = \operator ->
    when operator is
        Exact -> ""
        GreaterThan -> ">"
        GreaterThanOrEqualTo -> ">="
        LessThan -> "<"
        LessThanOrEqualTo -> "<="
        PatchUpdates -> "~"
        Compatible -> "^"

satisfies : Comparator, Semver -> Bool
satisfies = \comp, version ->
    when comp.operator is
        Exact -> matchesExact comp version
        GreaterThan -> matchesGreater comp version
        GreaterThanOrEqualTo -> matchesExact comp version || matchesGreater comp version
        LessThan -> matchesLess comp version
        LessThanOrEqualTo -> matchesExact comp version || matchesLess comp version
        PatchUpdates -> matchesPatchUpdates comp version
        Compatible -> matchesCompatible comp version

matchesWildcard : WildcardComparator, Semver -> Bool
matchesWildcard = \comp, version ->
    when comp is
        All -> Bool.true

matchesExact : Comparator, Semver -> Bool
matchesExact = \{ version }, { major, minor, patch, preRelease } ->
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
    when comp.version is
        Major { major: compMajor } if compMajor != major -> major > compMajor
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

expect matches testBaseComparator testBaseSemver

expect
    matches { testBaseComparator & operator: LessThan } testBaseSemver
    |> Bool.not

expect
    matches { testBaseComparator & operator: LessThanOrEqualTo } testBaseSemver

expect
    matches { testBaseComparator & operator: GreaterThan } testBaseSemver
    |> Bool.not

expect
    matches { testBaseComparator & operator: GreaterThanOrEqualTo } testBaseSemver

expect
    matches { testBaseComparator & operator: GreaterThanOrEqualTo } testBaseSemver

# TODO: write more tests for this

