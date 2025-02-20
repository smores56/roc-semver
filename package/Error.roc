module [
    invalid_number_error_to_str,
    invalid_identifier_error_to_str,
    invalid_semver_error_to_str,
    invalid_version_req_error_to_str,
    invalid_comparator_error_to_str,
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
invalid_number_error_to_str : InvalidNumberError -> Str
invalid_number_error_to_str = |error|
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
invalid_identifier_error_to_str : InvalidIdentifierError -> Str
invalid_identifier_error_to_str = |error|
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
invalid_semver_error_to_str : InvalidSemverError -> Str
invalid_semver_error_to_str = |error|
    when error is
        EmptySemver -> "The semver was empty"
        InvalidMajorVersion(num_err) -> "The major version was invalid: ${invalid_number_error_to_str(num_err)}"
        NoPeriodAfterMajorVersion -> "No period was found after the major version"
        InvalidMinorVersion(num_err) -> "The minor version was invalid: ${invalid_number_error_to_str(num_err)}"
        NoPeriodAfterMinorVersion -> "No period was found after the minor version"
        InvalidPatchVersion(num_err) -> "The patch version was invalid: ${invalid_number_error_to_str(num_err)}"
        InvalidPreRelease(id_err) -> "The pre-release was invalid: ${invalid_identifier_error_to_str(id_err)}"
        InvalidBuild(id_err) -> "The build metadata was invalid: ${invalid_identifier_error_to_str(id_err)}"
        UnexpectedSuffix(suffix) -> "An unexpected suffix '${suffix}' was found"

## The provided version requirement is invalid.
InvalidVersionReqError : [
    InvalidComparator { index : U64, error : InvalidComparatorError },
    TooManyComparators,
    UnexpectedSuffix Str,
]

## Render an InvalidVersionReqError to a string.
invalid_version_req_error_to_str : InvalidVersionReqError -> Str
invalid_version_req_error_to_str = |err|
    when err is
        TooManyComparators -> "Only 32 comparators are allowed in one version requirement"
        UnexpectedSuffix(suffix) -> "An unexpected suffix '${suffix}' was found"
        InvalidComparator({ index, error }) ->
            "Invalid comparator at index ${Num.to_str(index)}: ${invalid_comparator_error_to_str(error)}"

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
invalid_comparator_error_to_str : InvalidComparatorError -> Str
invalid_comparator_error_to_str = |error|
    when error is
        InvalidMajorVersion(num_err) -> "The major version was invalid: ${invalid_number_error_to_str(num_err)}"
        InvalidMinorConstraint(num_err) -> "The minor version was invalid: ${invalid_number_error_to_str(num_err)}"
        InvalidPatchConstraint(num_err) -> "The patch version was invalid: ${invalid_number_error_to_str(num_err)}"
        InvalidPreReleaseConstraint(id_err) -> "The pre-release was invalid: ${invalid_identifier_error_to_str(id_err)}"
        InvalidBuildConstraint(id_err) -> "The build metadata was invalid: ${invalid_identifier_error_to_str(id_err)}"
        UnexpectedSuffix(suffix) -> "An unexpected suffix '${suffix}' was found"
