module [
    invalidNumberErrorToStr,
    invalidIdentifierErrorToStr,
    invalidSemverErrorToStr,
    invalidVersionReqErrorToStr,
    invalidComparatorErrorToStr,
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

## Render an InvalidNumberError to a string.
invalidNumberErrorToStr : InvalidNumberError -> Str
invalidNumberErrorToStr = \error ->
    when error is
        Empty -> "no more text to parse"
        InvalidNumber -> "not a valid number"
        CannotStartWithZero -> "numeric identifier cannot start with zero"

## The provided identifier is invalid.
InvalidIdentifierError : [
    EmptySegment,
    InvalidIdentifier,
]

## Render an InvalidIdentifierError to a string.
invalidIdentifierErrorToStr : InvalidIdentifierError -> Str
invalidIdentifierErrorToStr = \error ->
    when error is
        EmptySegment -> "no more text to parse"
        InvalidIdentifier -> "invalid identifier found"

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

## Render an InvalidSemverError to a string.
invalidSemverErrorToStr : InvalidSemverError -> Str
invalidSemverErrorToStr = \error ->
    when error is
        EmptySemver -> "The semver was empty"
        InvalidMajorVersion numErr -> "The major version was invalid: $(invalidNumberErrorToStr numErr)"
        NoPeriodAfterMajorVersion -> "No period was found after the major version"
        InvalidMinorVersion numErr -> "The minor version was invalid: $(invalidNumberErrorToStr numErr)"
        NoPeriodAfterMinorVersion -> "No period was found after the minor version"
        InvalidPatchVersion numErr -> "The patch version was invalid: $(invalidNumberErrorToStr numErr)"
        InvalidPreRelease idErr -> "The pre-release was invalid: $(invalidIdentifierErrorToStr idErr)"
        InvalidBuild idErr -> "The build metadata was invalid: $(invalidIdentifierErrorToStr idErr)"
        UnexpectedSuffix suffix -> "An unexpected suffix '$(suffix)' was found"

## The provided version requirement is invalid.
InvalidVersionReqError : [
    InvalidComparator { index : U64, error : InvalidComparatorError },
    TooManyComparators,
    UnexpectedSuffix Str,
]

## Render an InvalidVersionReqError to a string.
invalidVersionReqErrorToStr : InvalidVersionReqError -> Str
invalidVersionReqErrorToStr = \err ->
    when err is
        TooManyComparators -> "Only 32 comparators are allowed in one version requirement"
        UnexpectedSuffix suffix -> "An unexpected suffix '$(suffix)' was found"
        InvalidComparator { index, error } ->
            "Invalid comparator at index $(Num.toStr index): $(invalidComparatorErrorToStr error)"

## The provided comparator is invalid.
InvalidComparatorError : [
    InvalidMajorVersion InvalidNumberError,
    InvalidMinorConstraint InvalidNumberError,
    InvalidPatchConstraint InvalidNumberError,
    InvalidPreReleaseConstraint InvalidIdentifierError,
    InvalidBuildConstraint InvalidIdentifierError,
    UnexpectedSuffix Str,
]

## Render an InvalidComparatorError to a string.
invalidComparatorErrorToStr : InvalidComparatorError -> Str
invalidComparatorErrorToStr = \error ->
    when error is
        InvalidMajorVersion numErr -> "The major version was invalid: $(invalidNumberErrorToStr numErr)"
        InvalidMinorConstraint numErr -> "The minor version was invalid: $(invalidNumberErrorToStr numErr)"
        InvalidPatchConstraint numErr -> "The patch version was invalid: $(invalidNumberErrorToStr numErr)"
        InvalidPreReleaseConstraint idErr -> "The pre-release was invalid: $(invalidIdentifierErrorToStr idErr)"
        InvalidBuildConstraint idErr -> "The build metadata was invalid: $(invalidIdentifierErrorToStr idErr)"
        UnexpectedSuffix suffix -> "An unexpected suffix '$(suffix)' was found"
