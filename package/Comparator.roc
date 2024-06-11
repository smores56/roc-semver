module [
    toStr,
    parse,
    parseLazy,
    accepts,
    preReleaseIsCompatible,
]

import Compare exposing [
    comparePreReleases,
    compareIfEqual,
]
import Error exposing [
    InvalidComparatorError,
]
import Types exposing [
    Semver,
    Comparator,
    ComparatorOperator,
    WildcardComparator,
    RelationComparatorVersion,
    Ordering,
]
import Parse

## Render a comparator to a string.
##
## ```roc
## wildcardComparator = Wildcard (MajorMinor 1 5)
## expect toStr wildcardComparator == "1.5.*"
##
## relationComparator = Relation { operator: GreaterThan, version: Major 2 }
## expect toStr relationComparator == ">2"
## ```
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
                    Major major -> "$(Num.toStr major)"
                    MajorMinor major minor -> "$(Num.toStr major).$(Num.toStr minor)"
                    Full { major, minor, patch, preRelease } ->
                        preReleaseStr =
                            if List.isEmpty preRelease then
                                ""
                            else
                                "-$(Str.joinWith preRelease ".")"

                        "$(Num.toStr major).$(Num.toStr minor).$(Num.toStr patch)$(preReleaseStr)"

            "$(operatorToStr operator)$(versionStr)"

operatorToStr : ComparatorOperator -> Str
operatorToStr = \operator ->
    when operator is
        Exact -> ""
        GreaterThan -> ">"
        GreaterThanOrEqualTo -> ">="
        LessThan -> "<"
        LessThanOrEqualTo -> "<="
        PatchUpdates -> "~"
        Compatible -> "^"

## Parse a comparator from a string.
##
## This fails if there is any leftover text after the string. Use the [parseLazy]
## twin function if you want to get the leftover text after parsing.
##
## ```roc
## comparator = parse ">=1.2"
##
## expect comparator == Ok (Relation { operator: GreaterThanOrEqualTo, version: MajorMinor 1 2 })
## ```
parse : Str -> Result Comparator InvalidComparatorError
parse = Parse.comparator

## Parse a comparator from a string, returning the leftover text after parsing on success.
##
## If you don't need the leftover text, use the [parse] twin function.
##
## ```roc
## comparator = parseLazy ">=1.2 abc"
##
## expect comparator == Ok (
##     Relation { operator: GreaterThanOrEqualTo, version: MajorMinor 1 2 }),
##     " abc",
## )
## ```
parseLazy : Str -> Result Comparator InvalidComparatorError
parseLazy = Parse.comparator

## Whether a semver satisfies the constraint of a comparator.
##
## _note: you should not use this method directly, as there are some checks_
## _done by [VersionReq.matches] not done here._
##
## There are two types of comparator:
## - Wildcard: these check that the specified segments of the comparator
##   match the semver, and all other segments are ignored.
## - Relation: these check that the specified segments of the comparator
##   hold the relation specified by the operator.
##
## ```roc
## comparator = Relation { operator: LessThan, version: MajorMinor 1 3 }
## version = { major: 1, minor: 2, patch: 10, preRelease: [], build: [] }
##
## expect comparator |> matches version
## ```
accepts : Comparator, Semver -> Bool
accepts = \comp, semver ->
    when comp is
        Wildcard constraint -> acceptsWildcard constraint semver
        Relation { version, operator } ->
            when operator is
                Exact -> compareRelationToVersion version semver == EQ
                GreaterThan -> compareRelationToVersion version semver == GT
                GreaterThanOrEqualTo -> compareRelationToVersion version semver != LT
                LessThan -> compareRelationToVersion version semver == LT
                LessThanOrEqualTo -> compareRelationToVersion version semver != GT
                PatchUpdates -> acceptsPatchUpdates version semver
                Compatible -> acceptsCompatible version semver

acceptsWildcard : WildcardComparator, Semver -> Bool
acceptsWildcard = \comp, version ->
    when comp is
        Full -> Bool.true
        Major major -> version.major == major
        MajorMinor major minor -> version.major == major && version.minor == minor

compareRelationToVersion : RelationComparatorVersion, Semver -> Ordering
compareRelationToVersion = \comp, version ->
    (compMajor, compMinor, compPatch, compPreRelease) =
        when comp is
            Major major -> (major, version.minor, version.patch, version.preRelease)
            MajorMinor major minor -> (major, minor, version.patch, version.preRelease)
            Full { major, minor, patch, preRelease } -> (major, minor, patch, preRelease)

    Num.compare version.major compMajor
    |> compareIfEqual \{} -> Num.compare version.minor compMinor
    |> compareIfEqual \{} -> Num.compare version.patch compPatch
    |> compareIfEqual \{} -> comparePreReleases version.preRelease compPreRelease

acceptsPatchUpdates : RelationComparatorVersion, Semver -> Bool
acceptsPatchUpdates = \comp, version ->
    when comp is
        Major major ->
            major == version.major

        MajorMinor major minor ->
            major == version.major && minor == version.minor

        Full { major, minor, patch, preRelease } ->
            (major == version.major)
            && (minor == version.minor)
            && (patch <= version.patch)
            && (comparePreReleases preRelease version.preRelease != GT)

## Checks if a version is API-compatible with a comparator.
##
## The spec for semver v2.0.0 explains compatibility: <https://semver.org>
acceptsCompatible : RelationComparatorVersion, Semver -> Bool
acceptsCompatible = \comp, version ->
    when comp is
        Major major ->
            major == version.major

        MajorMinor major minor ->
            if major != version.major then
                Bool.false
            else if major > 0 then
                version.minor >= minor
            else
                version.minor == minor

        Full { major, minor, patch, preRelease } ->
            if major != version.major then
                Bool.false
            else if major > 0 && version.minor != minor then
                version.minor > minor
            else if major > 0 && version.patch != patch then
                version.patch > patch
            else if minor > 0 && version.minor != minor then
                Bool.false
            else if minor > 0 && version.patch != patch then
                version.patch > patch
            else if version.minor != minor || version.patch != patch then
                Bool.false
            else
                comparePreReleases version.preRelease preRelease != LT

## Checks if a semver has a non-empty pre-release and matches all of
## its version segments.
preReleaseIsCompatible : Comparator, Semver -> Bool
preReleaseIsCompatible = \comparator, semver ->
    when comparator is
        Wildcard _wildcard -> Bool.false
        Relation { version, operator: _ } ->
            when version is
                Major _major | MajorMinor _major _minor -> Bool.false
                Full { major, minor, patch, preRelease } ->
                    (major == semver.major)
                    && (minor == semver.minor)
                    && (patch == semver.patch)
                    && !(List.isEmpty preRelease)

testBaseSemver = { major: 1, minor: 2, patch: 3, preRelease: [], build: [] }
testBaseCompVer = { major: 1, minor: 2, patch: 3, preRelease: [] }
testBaseComparator = Relation { operator: Exact, version: Full testBaseCompVer }

expect accepts testBaseComparator testBaseSemver

expect
    comp = Relation { operator: LessThan, version: Full testBaseCompVer }
    comp |> accepts testBaseSemver == Bool.false

expect
    comp = Relation { operator: LessThanOrEqualTo, version: Full testBaseCompVer }
    comp |> accepts testBaseSemver == Bool.true

expect
    comp = Relation { operator: GreaterThan, version: Full testBaseCompVer }
    comp |> accepts testBaseSemver == Bool.false

expect
    comp = Relation { operator: GreaterThanOrEqualTo, version: Full testBaseCompVer }
    comp |> accepts testBaseSemver == Bool.true

expect
    comp = Relation { operator: PatchUpdates, version: Full testBaseCompVer }
    comp |> accepts testBaseSemver == Bool.true

expect
    comp = Relation { operator: Compatible, version: Full testBaseCompVer }
    comp |> accepts testBaseSemver == Bool.true
