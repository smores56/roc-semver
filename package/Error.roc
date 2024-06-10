module [
    InvalidNumberError,
    InvalidIdentifierError,
    InvalidSemverError,
    InvalidVersionReqError,
    InvalidComparatorError,
]

## The provided numeric identifier is invalid.
InvalidNumberError : [
    Empty,
    InvalidNumber,
    CannotStartWithZero,
]

## The provided identifier is invalid.
InvalidIdentifierError : [
    EmptySegment,
    InvalidIdentifier,
]

## The provided semantic version is invalid.
InvalidSemverError : [
    EmptySemver,
    InvalidMajorVersion InvalidNumberError,
    NoPeriodAfterMajorVersion,
    InvalidMinorVersion InvalidNumberError,
    NoPeriodAfterMinorVersion,
    InvalidPatchVersion InvalidNumberError,
    InvalidPreRelease InvalidIdentifierError,
    InvalidBuild InvalidIdentifierError,
    UnexpectedSuffix Str,
]

## The provided version requirement is invalid.
InvalidVersionReqError : [
    InvalidComparator { index : U64, error : InvalidComparatorError },
    TooManyComparators,
    UnexpectedSuffix Str,
]

## The provided comparator is invalid.
InvalidComparatorError : [
    InvalidMajorVersion InvalidNumberError,
    InvalidMinorConstraint InvalidNumberError,
    InvalidPatchConstraint InvalidNumberError,
    PatchSpecifiedAfterMinorWildcard,
    InvalidPreReleaseConstraint InvalidIdentifierError,
    InvalidBuildConstraint InvalidIdentifierError,
    UnexpectedSuffix Str,
]
