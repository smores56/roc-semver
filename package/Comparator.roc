module [
    to_str,
    parse,
    parse_lazy,
    accepts,
    pre_release_is_compatible,
]

import Compare exposing [
    compare_pre_releases,
    compare_if_equal,
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
## wildcard_comparator = Wildcard(MajorMinor(1, 5))
## expect to_str(wildcard_comparator) == "1.5.*"
##
## relation_comparator = Relation({ operator: GreaterThan, version: Major(2) })
## expect to_str(relation_comparator) == ">2"
## ```
to_str : Comparator -> Str
to_str = |comp|
    when comp is
        Wildcard(version) ->
            when version is
                Full -> "*"
                Major(major) -> "${Num.to_str(major)}.*"
                MajorMinor(major, minor) -> "${Num.to_str(major)}.${Num.to_str(minor)}.*"

        Relation({ version, operator }) ->
            version_str =
                when version is
                    Major(major) -> "${Num.to_str(major)}"
                    MajorMinor(major, minor) -> "${Num.to_str(major)}.${Num.to_str(minor)}"
                    Full({ major, minor, patch, pre_release }) ->
                        pre_release_str =
                            if List.is_empty(pre_release) then
                                ""
                            else
                                "-${Str.join_with(pre_release, ".")}"

                        "${Num.to_str(major)}.${Num.to_str(minor)}.${Num.to_str(patch)}${pre_release_str}"

            "${operator_to_str(operator)}${version_str}"

operator_to_str : ComparatorOperator -> Str
operator_to_str = |operator|
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
## comparator = parse(">=1.2")
##
## expect comparator == Ok(Relation({ operator: GreaterThanOrEqualTo, version: MajorMinor(1, 2) }))
## ```
parse : Str -> Result Comparator InvalidComparatorError
parse = Parse.comparator

## Parse a comparator from a string, returning the leftover text after parsing on success.
##
## If you don't need the leftover text, use the [parse] twin function.
##
## ```roc
## comparator = parse_lazy(">=1.2 abc")
##
## expect 
##     comparator == Ok (
##     Relation { operator: GreaterThanOrEqualTo, version: MajorMinor 1 2 }
## )
## ```
parse_lazy : Str -> Result Comparator InvalidComparatorError
parse_lazy = Parse.comparator

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
## comparator = Relation({ operator: LessThan, version: MajorMinor 1 3 })
## version = { major: 1, minor: 2, patch: 10, pre_release: [], build: [] }
##
## expect [comparator] |> matches(version)
## ```
accepts : Comparator, Semver -> Bool
accepts = |comp, semver|
    when comp is
        Wildcard(constraint) -> accepts_wildcard(constraint, semver)
        Relation({ version, operator }) ->
            when operator is
                Exact -> compare_relation_to_version(version, semver) == EQ
                GreaterThan -> compare_relation_to_version(version, semver) == GT
                GreaterThanOrEqualTo -> compare_relation_to_version(version, semver) != LT
                LessThan -> compare_relation_to_version(version, semver) == LT
                LessThanOrEqualTo -> compare_relation_to_version(version, semver) != GT
                PatchUpdates -> accepts_patch_updates(version, semver)
                Compatible -> accepts_compatible(version, semver)

accepts_wildcard : WildcardComparator, Semver -> Bool
accepts_wildcard = |comp, version|
    when comp is
        Full -> Bool.true
        Major(major) -> version.major == major
        MajorMinor(major, minor) -> version.major == major and version.minor == minor

compare_relation_to_version : RelationComparatorVersion, Semver -> Ordering
compare_relation_to_version = |comp, version|
    (comp_major, comp_minor, comp_patch, comp_pre_release) =
        when comp is
            Major(major) -> (major, version.minor, version.patch, version.pre_release)
            MajorMinor(major, minor) -> (major, minor, version.patch, version.pre_release)
            Full({ major, minor, patch, pre_release }) -> (major, minor, patch, pre_release)

    Num.compare(version.major, comp_major)
    |> compare_if_equal(|{}| Num.compare(version.minor, comp_minor))
    |> compare_if_equal(|{}| Num.compare(version.patch, comp_patch))
    |> compare_if_equal(|{}| compare_pre_releases(version.pre_release, comp_pre_release))

accepts_patch_updates : RelationComparatorVersion, Semver -> Bool
accepts_patch_updates = |comp, version|
    when comp is
        Major(major) ->
            major == version.major

        MajorMinor(major, minor) ->
            major == version.major and minor == version.minor

        Full({ major, minor, patch, pre_release }) ->
            (major == version.major)
            and (minor == version.minor)
            and (patch <= version.patch)
            and (compare_pre_releases(pre_release, version.pre_release) != GT)

## Checks if a version is API-compatible with a comparator.
##
## The spec for semver v2.0.0 explains compatibility: <https://semver.org>
accepts_compatible : RelationComparatorVersion, Semver -> Bool
accepts_compatible = |comp, version|
    when comp is
        Major(major) ->
            major == version.major

        MajorMinor(major, minor) ->
            if major != version.major then
                Bool.false
            else if major > 0 then
                version.minor >= minor
            else
                version.minor == minor

        Full({ major, minor, patch, pre_release }) ->
            if major != version.major then
                Bool.false
            else if major > 0 and version.minor != minor then
                version.minor > minor
            else if major > 0 and version.patch != patch then
                version.patch > patch
            else if minor > 0 and version.minor != minor then
                Bool.false
            else if minor > 0 and version.patch != patch then
                version.patch > patch
            else if version.minor != minor or version.patch != patch then
                Bool.false
            else
                compare_pre_releases(version.pre_release, pre_release) != LT

## Checks if a semver has a non-empty pre-release and matches all of
## its version segments.
pre_release_is_compatible : Comparator, Semver -> Bool
pre_release_is_compatible = |comparator, semver|
    when comparator is
        Wildcard(_wildcard) -> Bool.false
        Relation({ version, operator: _ }) ->
            when version is
                Major(_major) | MajorMinor(_major, _minor) -> Bool.false
                Full({ major, minor, patch, pre_release }) ->
                    (major == semver.major)
                    and (minor == semver.minor)
                    and (patch == semver.patch)
                    and !(List.is_empty(pre_release))

test_base_semver = { major: 1, minor: 2, patch: 3, pre_release: [], build: [] }
test_base_comp_ver = { major: 1, minor: 2, patch: 3, pre_release: [] }
test_base_comparator = Relation({ operator: Exact, version: Full(test_base_comp_ver) })

expect accepts(test_base_comparator, test_base_semver)

expect
    comp = Relation({ operator: LessThan, version: Full(test_base_comp_ver) })
    comp |> accepts(test_base_semver) == Bool.false

expect
    comp = Relation({ operator: LessThanOrEqualTo, version: Full(test_base_comp_ver) })
    comp |> accepts(test_base_semver) == Bool.true

expect
    comp = Relation({ operator: GreaterThan, version: Full(test_base_comp_ver) })
    comp |> accepts(test_base_semver) == Bool.false

expect
    comp = Relation({ operator: GreaterThanOrEqualTo, version: Full(test_base_comp_ver) })
    comp |> accepts(test_base_semver) == Bool.true

expect
    comp = Relation({ operator: PatchUpdates, version: Full(test_base_comp_ver) })
    comp |> accepts(test_base_semver) == Bool.true

expect
    comp = Relation({ operator: Compatible, version: Full(test_base_comp_ver) })
    comp |> accepts(test_base_semver) == Bool.true
