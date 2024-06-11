module [
    compareSemvers,
    comparePreReleases,
    compareIfEqual,
]

import Types exposing [Semver, Ordering]

compareSemvers : Semver, Semver -> Ordering
compareSemvers = \left, right ->
    Num.compare left.major right.major
    |> compareIfEqual \{} -> Num.compare left.minor right.minor
    |> compareIfEqual \{} -> Num.compare left.patch right.patch
    |> compareIfEqual \{} -> comparePreReleases left.preRelease right.preRelease

comparePreReleases : List Str, List Str -> Ordering
comparePreReleases = \left, right ->
    if List.isEmpty left then
        if List.isEmpty right then
            EQ
        else
            # A real release compares greater than prerelease.
            GT
    else if List.isEmpty right then
        # Prerelease compares less than the real release.
        LT
    else
        orderings =
            List.map2 left right Pair
            |> List.keepOks \Pair leftSegment rightSegment ->
                compareIdentifiers leftSegment rightSegment
                |> onlyAllowDifferentOrdering

        when List.first orderings is
            Ok ordering -> ordering
            Err ListWasEmpty -> Num.compare (List.len left) (List.len right)

compareIdentifiers : Str, Str -> Ordering
compareIdentifiers = \left, right ->
    when (Str.toU64 left, Str.toU64 right) is
        # "Identifiers consisting of only digits are compared numerically."
        (Ok leftNum, Ok rightNum) ->
            Num.compare leftNum rightNum
            |> compareIfEqual \{} -> Num.compare (strLen left) (strLen right)

        # "Numeric identifiers always have lower precedence than non-numeric identifiers."
        (Ok _leftNum, Err _) -> LT
        (Err _, Ok _rightNum) -> GT
        # "Identifiers with letters or hyphens are compared lexically in ASCII sort order."
        (Err _, Err _) -> compareStrings left right

onlyAllowDifferentOrdering : Ordering -> Result Ordering [OrderingIsEqual]
onlyAllowDifferentOrdering = \ordering ->
    when ordering is
        EQ -> Err OrderingIsEqual
        different -> Ok different

compareIfEqual : [LT, GT, EQ], ({} -> [LT, GT, EQ]) -> [LT, GT, EQ]
compareIfEqual = \ordering, comparator ->
    if ordering == EQ then
        comparator {}
    else
        ordering

strLen : Str -> U64
strLen = \s ->
    List.len (Str.toUtf8 s)

compareStrings : Str, Str -> [LT, GT, EQ]
compareStrings = \left, right ->
    leftBytes = Str.toUtf8 left
    rightBytes = Str.toUtf8 right

    orderings =
        List.map2 leftBytes rightBytes Pair
        |> List.keepOks \Pair l r ->
            Num.compare l r
            |> onlyAllowDifferentOrdering

    when List.first orderings is
        Ok ordering -> ordering
        Err ListWasEmpty ->
            Num.compare (List.len leftBytes) (List.len rightBytes)

baseTestSemver : Semver
baseTestSemver = { major: 1, minor: 2, patch: 3, preRelease: [], build: [] }

expect compareSemvers baseTestSemver baseTestSemver == EQ

# major version is compared first
expect compareSemvers baseTestSemver { baseTestSemver & major: 2 } == LT
expect compareSemvers baseTestSemver { baseTestSemver & major: 0 } == GT

# minor version is compared if major versions are the same
expect compareSemvers baseTestSemver { baseTestSemver & minor: 1 } == GT
expect compareSemvers baseTestSemver { baseTestSemver & minor: 3 } == LT
expect compareSemvers baseTestSemver { baseTestSemver & minor: 3, major: 0 } == GT

# patch version is compared if major and minor versions are the same
expect compareSemvers baseTestSemver { baseTestSemver & patch: 2 } == GT
expect compareSemvers baseTestSemver { baseTestSemver & patch: 4 } == LT
expect compareSemvers baseTestSemver { baseTestSemver & patch: 4, major: 0 } == GT
expect compareSemvers baseTestSemver { baseTestSemver & patch: 4, minor: 1 } == GT

# prereleases are compared if major, minor, and patch versions match
expect compareSemvers { baseTestSemver & preRelease: ["alpha"] } baseTestSemver == LT
expect compareSemvers baseTestSemver { baseTestSemver & preRelease: ["alpha"], major: 0 } == GT
expect compareSemvers baseTestSemver { baseTestSemver & preRelease: ["alpha"], minor: 1 } == GT
expect compareSemvers baseTestSemver { baseTestSemver & preRelease: ["alpha"], patch: 2 } == GT

# build metadata isn't considered for comparison
expect compareSemvers baseTestSemver { baseTestSemver & build: ["alpha"] } == EQ

expect comparePreReleases [] [] == EQ

# prereleases are before "real" releases (a.k.a. versions with no prerelease)
expect comparePreReleases ["alpha"] [] == LT
expect comparePreReleases [] ["alpha"] == GT

# So long as all common segments match, the longer prerelease is preferred
expect comparePreReleases ["a"] ["a"] == EQ
expect comparePreReleases ["a", "a"] ["a"] == GT
expect comparePreReleases ["a"] ["a", "a"] == LT

# When both are numbers, compare their values
expect comparePreReleases ["1"] ["2"] == LT
expect comparePreReleases ["2"] ["1"] == GT
expect comparePreReleases ["10"] ["00"] == GT

# Length is compared when numbers are equivalent
expect comparePreReleases ["0"] ["00"] == LT
expect comparePreReleases ["00"] ["0"] == GT

# Numbers are weighted less than text
expect comparePreReleases ["0"] ["a"] == LT
expect comparePreReleases ["a"] ["0"] == GT

# Text is compared lexicographically if neither are numbers
expect comparePreReleases ["abc"] ["def"] == LT
expect comparePreReleases ["def"] ["abc"] == GT
expect comparePreReleases ["a"] ["A"] == GT
expect comparePreReleases ["9"] ["Z"] == LT
expect comparePreReleases ["a"] ["ab"] == LT
expect comparePreReleases ["ab"] ["a"] == GT
