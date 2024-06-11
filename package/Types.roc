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

## A semantic version, as specified in v2.0.0 of the spec: <https://semver.org>
Semver : {
    major : U64,
    minor : U64,
    patch : U64,
    build : List Str,
    preRelease : List Str,
}

## A version requirement constraining semvers.
VersionReq : List Comparator

## A predicate to match versions against.
Comparator : [
    Wildcard WildcardComparator,
    Relation RelationComparator,
]

## A comparator with wildcard segments.
WildcardComparator : [
    Full,
    Major U64,
    MajorMinor U64 U64,
]

## A comparator that compares a partial version using some operator.
RelationComparator : {
    version : RelationComparatorVersion,
    operator : ComparatorOperator,
}

## The version that a relation comparator compares semvers against.
RelationComparatorVersion : [
    Major U64,
    MajorMinor U64 U64,
    Full
        {
            major : U64,
            minor : U64,
            patch : U64,
            preRelease : List Str,
        },
]

## The operator to determine how to compare a version to a comparator.
##
## - `Exact`: must match all segments exactly
## - `GreaterThan`: must be greater than the constraint version
## - `GreaterThanOrEqualTo`: must be greater than or equal to the constraint version
## - `LessThan`: must be less than the constraint version
## - `LessThanOrEqualTo`: must be less than or equal to the constraint version
## - `PatchUpdates`: must be the same major and minor version, but allows patch updates
## - `Compatible`: must be API compatible with the constraint version
ComparatorOperator : [
    Exact,
    GreaterThan,
    GreaterThanOrEqualTo,
    LessThan,
    LessThanOrEqualTo,
    PatchUpdates,
    Compatible,
]

## How two items are ordered.
Ordering : [LT, GT, EQ]
