module [
    Semver,
    VersionReq,
    Comparator,
    WildcardComparator,
    RelationComparator,
    RelationComparatorVersion,
    ComparatorOperator,
    Ordering,
]

Semver : {
    major : U64,
    minor : U64,
    patch : U64,
    build : List Str,
    preRelease : List Str,
}

VersionReq : List Comparator

## A predicate to match versions against.
Comparator : [
    Wildcard WildcardComparator,
    Relation RelationComparator,
]

WildcardComparator : [
    Full,
    Major U64,
    MajorMinor U64 U64,
]

RelationComparatorVersion : [
    Major U64,
    MajorMinor U64 U64,
    MajorMinorPatch U64 U64 U64,
    MajorMinorPatchPreRelease U64 U64 U64 (List Str),
]

RelationComparator : {
    version : RelationComparatorVersion,
    operator : ComparatorOperator,
}

## The operator to determine how to compare a version to a comparator.
ComparatorOperator : [
    Exact,
    GreaterThan,
    GreaterThanOrEqualTo,
    LessThan,
    LessThanOrEqualTo,
    PatchUpdates,
    Compatible,
]

Ordering : [LT, GT, EQ]
