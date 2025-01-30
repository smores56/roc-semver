module [
    compare_semvers,
    compare_pre_releases,
    compare_if_equal,
]

import Types exposing [Semver, Ordering]

compare_semvers : Semver, Semver -> Ordering
compare_semvers = |left, right|
    Num.compare(left.major, right.major)
    |> compare_if_equal(|{}| Num.compare(left.minor, right.minor))
    |> compare_if_equal(|{}| Num.compare(left.patch, right.patch))
    |> compare_if_equal(|{}| compare_pre_releases(left.pre_release, right.pre_release))

compare_pre_releases : List Str, List Str -> Ordering
compare_pre_releases = |left, right|
    if List.is_empty(left) then
        if List.is_empty(right) then
            EQ
        else
            # A real release compares greater than prerelease.
            GT
    else if List.is_empty(right) then
        # Prerelease compares less than the real release.
        LT
    else
        orderings =
            List.map2(left, right, Pair)
            |> List.keep_oks(
                |Pair(left_segment, right_segment)|
                    compare_identifiers(left_segment, right_segment)
                    |> only_allow_different_ordering,
            )

        when List.first(orderings) is
            Ok(ordering) -> ordering
            Err(ListWasEmpty) -> Num.compare(List.len(left), List.len(right))

compare_identifiers : Str, Str -> Ordering
compare_identifiers = |left, right|
    when (Str.to_u64(left), Str.to_u64(right)) is
        # "Identifiers consisting of only digits are compared numerically."
        (Ok(left_num), Ok(right_num)) ->
            Num.compare(left_num, right_num)
            |> compare_if_equal(|{}| Num.compare(str_len(left), str_len(right)))

        # "Numeric identifiers always have lower precedence than non-numeric identifiers."
        (Ok(_leftNum), Err(_)) -> LT
        (Err(_), Ok(_rightNum)) -> GT
        # "Identifiers with letters or hyphens are compared lexically in ASCII sort order."
        (Err(_), Err(_)) -> compare_strings(left, right)

only_allow_different_ordering : Ordering -> Result Ordering [OrderingIsEqual]
only_allow_different_ordering = |ordering|
    when ordering is
        EQ -> Err(OrderingIsEqual)
        different -> Ok(different)

compare_if_equal : [LT, GT, EQ], ({} -> [LT, GT, EQ]) -> [LT, GT, EQ]
compare_if_equal = |ordering, comparator|
    if ordering == EQ then
        comparator({})
    else
        ordering

str_len : Str -> U64
str_len = |s|
    List.len(Str.to_utf8(s))

compare_strings : Str, Str -> [LT, GT, EQ]
compare_strings = |left, right|
    left_bytes = Str.to_utf8(left)
    right_bytes = Str.to_utf8(right)

    orderings =
        List.map2(left_bytes, right_bytes, Pair)
        |> List.keep_oks(
            |Pair(l, r)|
                Num.compare(l, r)
                |> only_allow_different_ordering,
        )

    when List.first(orderings) is
        Ok(ordering) -> ordering
        Err(ListWasEmpty) ->
            Num.compare(List.len(left_bytes), List.len(right_bytes))

base_test_semver : Semver
base_test_semver = { major: 1, minor: 2, patch: 3, pre_release: [], build: [] }

expect compare_semvers(base_test_semver, base_test_semver) == EQ

# major version is compared first
expect compare_semvers(base_test_semver, { base_test_semver & major: 2 }) == LT
expect compare_semvers(base_test_semver, { base_test_semver & major: 0 }) == GT

# minor version is compared if major versions are the same
expect compare_semvers(base_test_semver, { base_test_semver & minor: 1 }) == GT
expect compare_semvers(base_test_semver, { base_test_semver & minor: 3 }) == LT
expect compare_semvers(base_test_semver, { base_test_semver & minor: 3, major: 0 }) == GT

# patch version is compared if major and minor versions are the same
expect compare_semvers(base_test_semver, { base_test_semver & patch: 2 }) == GT
expect compare_semvers(base_test_semver, { base_test_semver & patch: 4 }) == LT
expect compare_semvers(base_test_semver, { base_test_semver & patch: 4, major: 0 }) == GT
expect compare_semvers(base_test_semver, { base_test_semver & patch: 4, minor: 1 }) == GT

# prereleases are compared if major, minor, and patch versions match
expect compare_semvers({ base_test_semver & pre_release: ["alpha"] }, base_test_semver) == LT
expect compare_semvers(base_test_semver, { base_test_semver & pre_release: ["alpha"], major: 0 }) == GT
expect compare_semvers(base_test_semver, { base_test_semver & pre_release: ["alpha"], minor: 1 }) == GT
expect compare_semvers(base_test_semver, { base_test_semver & pre_release: ["alpha"], patch: 2 }) == GT

# build metadata isn't considered for comparison
expect compare_semvers(base_test_semver, { base_test_semver & build: ["alpha"] }) == EQ

expect compare_pre_releases([], []) == EQ

# prereleases are before "real" releases (a.k.a. versions with no prerelease)
expect compare_pre_releases(["alpha"], []) == LT
expect compare_pre_releases([], ["alpha"]) == GT

# So long as all common segments match, the longer prerelease is preferred
expect compare_pre_releases(["a"], ["a"]) == EQ
expect compare_pre_releases(["a", "a"], ["a"]) == GT
expect compare_pre_releases(["a"], ["a", "a"]) == LT

# When both are numbers, compare their values
expect compare_pre_releases(["1"], ["2"]) == LT
expect compare_pre_releases(["2"], ["1"]) == GT
expect compare_pre_releases(["10"], ["00"]) == GT

# Length is compared when numbers are equivalent
expect compare_pre_releases(["0"], ["00"]) == LT
expect compare_pre_releases(["00"], ["0"]) == GT

# Numbers are weighted less than text
expect compare_pre_releases(["0"], ["a"]) == LT
expect compare_pre_releases(["a"], ["0"]) == GT

# Text is compared lexicographically if neither are numbers
expect compare_pre_releases(["abc"], ["def"]) == LT
expect compare_pre_releases(["def"], ["abc"]) == GT
expect compare_pre_releases(["a"], ["A"]) == GT
expect compare_pre_releases(["9"], ["Z"]) == LT
expect compare_pre_releases(["a"], ["ab"]) == LT
expect compare_pre_releases(["ab"], ["a"]) == GT
