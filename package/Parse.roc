module [
    semver,
    semver_lazy,
    comparator,
    comparator_lazy,
    version_req,
    version_req_lazy,
]

import Types exposing [
    Semver,
    VersionReq,
    Comparator,
    ComparatorOperator,
]
import Error exposing [
    InvalidSemverError,
    InvalidVersionReqError,
    InvalidComparatorError,
    InvalidNumberError,
    InvalidIdentifierError,
]

max_comparator_count = 32

semver_lazy : Str -> Result (Semver, Str) InvalidSemverError
semver_lazy = |s|
    assert_semver_not_empty(s)?

    bytes = Str.to_utf8(s)

    (major, after_major) =
        parse_numeric_identifier(bytes)
        |> Result.map_err(InvalidMajorVersion)?
    after_major_dot =
        parse_byte(after_major, '.')
        |> Result.map_err(|ByteNotFound| NoPeriodAfterMajorVersion)?

    (minor, after_minor) =
        parse_numeric_identifier(after_major_dot)
        |> Result.map_err(InvalidMinorVersion)?
    after_minor_dot =
        parse_byte(after_minor, '.')
        |> Result.map_err(|ByteNotFound| NoPeriodAfterMinorVersion)?

    (patch, after_patch) =
        parse_numeric_identifier(after_minor_dot)
        |> Result.map_err(InvalidPatchVersion)?

    (pre_release, after_pre_release) =
        parse_optional_pre_release(after_patch)
        |> Result.map_err(InvalidPreRelease)?

    (build, after_build) =
        parse_optional_build(after_pre_release)
        |> Result.map_err(InvalidBuild)?

    Ok(
        (
            {
                major,
                minor,
                patch,
                build,
                pre_release,
            },
            after_build
            |> Str.from_utf8
            |> Result.with_default(""),
        ),
    )

semver : Str -> Result Semver InvalidSemverError
semver = |s|
    (version, rest) = semver_lazy(s)?

    if Str.is_empty(rest) then
        Ok(version)
    else
        Err(UnexpectedSuffix(rest))

assert_semver_not_empty : Str -> Result {} [EmptySemver]
assert_semver_not_empty = |s|
    if Str.is_empty(s) then
        Err(EmptySemver)
    else
        Ok({})

parse_optional_pre_release : List U8 -> Result (List Str, List U8) InvalidIdentifierError
parse_optional_pre_release = |chars|
    when chars is
        [first, .. as rest] if first == '-' -> parse_pre_release(rest)
        _other -> Ok(([], chars))

parse_pre_release : List U8 -> Result (List Str, List U8) InvalidIdentifierError
parse_pre_release = |chars|
    parse_pre_release_identifier = |id_chars|
        when parse_alphanumeric_identifier(id_chars) is
            Ok((identifier, rest)) -> Ok((identifier, rest))
            Err(MustHaveNonDigitChars) ->
                when parse_numeric_identifier(id_chars) is
                    Ok((num, rest)) -> Ok((Num.to_str(num), rest))
                    Err(err) ->
                        when err is
                            Empty ->
                                Err(EmptySegment)

                            CannotStartWithZero | InvalidNumber ->
                                Err(InvalidIdentifier)

    take_remaining_pre_release_identifiers = |tail_chars|
        when tail_chars is
            [first, .. as rest] if first == '.' ->
                (identifier, remaining) =
                    parse_pre_release_identifier(rest)?
                (found_identifiers, leftover) =
                    take_remaining_pre_release_identifiers(remaining)?

                Ok((List.concat([identifier], found_identifiers), leftover))

            _other -> Ok(([], tail_chars))

    when parse_pre_release_identifier(chars) is
        Ok((first_identifier, after_first_identifier)) ->
            (remaining_identifiers, rest) =
                take_remaining_pre_release_identifiers(after_first_identifier)?

            Ok((List.concat([first_identifier], remaining_identifiers), rest))

        Err(err) ->
            if err == EmptySegment and !(List.is_empty(chars)) then
                Err(InvalidIdentifier)
            else
                Err(err)

parse_optional_build : List U8 -> Result (List Str, List U8) InvalidIdentifierError
parse_optional_build = |chars|
    when chars is
        [first, .. as rest] if first == '+' -> parse_build(rest)
        _other -> Ok(([], chars))

parse_build : List U8 -> Result (List Str, List U8) InvalidIdentifierError
parse_build = |chars|
    parse_build_identifier = |id_chars|
        when parse_alphanumeric_identifier(id_chars) is
            Ok((identifier, rest)) -> Ok((identifier, rest))
            Err(MustHaveNonDigitChars) ->
                (digits, leftover) = take_chars_while(id_chars, is_digit)
                if List.is_empty(digits) then
                    Err(EmptySegment)
                else
                    Ok((digits |> Str.from_utf8 |> Result.with_default(""), leftover))

    take_remaining_build_identifiers = |tail_chars|
        when tail_chars is
            [first, .. as rest] if first == '.' ->
                (identifier, remaining) =
                    parse_build_identifier(rest)?
                (found_identifiers, leftover) =
                    take_remaining_build_identifiers(remaining)?
                Ok((List.concat([identifier], found_identifiers), leftover))

            _other -> Ok(([], tail_chars))

    when parse_build_identifier(chars) is
        Ok((first_identifier, after_first_identifier)) ->
            (remaining_identifiers, rest) =
                take_remaining_build_identifiers(after_first_identifier)?

            Ok((List.concat([first_identifier], remaining_identifiers), rest))

        Err(err) ->
            if err == EmptySegment and !(List.is_empty(chars)) then
                Err(InvalidIdentifier)
            else
                Err(err)

version_req_lazy : Str -> Result (VersionReq, Str) InvalidVersionReqError
version_req_lazy = |s|
    take_comparators_while_present = |tail_chars, req_index|
        when tail_chars is
            [first, .. as rest] if first == ',' ->
                if req_index >= max_comparator_count then
                    Err(TooManyComparators)
                else
                    tail_after_spaces = drop_chars_while(rest, is_space)
                    (comp, after_comp) =
                        parse_comparator(tail_after_spaces)
                        |> Result.map_err(|error| InvalidComparator({ index: req_index, error }))?
                    (comps, after_comps) =
                        take_comparators_while_present(after_comp, (req_index + 1))?

                    Ok((List.concat([comp], comps), after_comps))

            _otherwise -> Ok(([], tail_chars))

    after_spaces =
        Str.to_utf8(s)
        |> drop_chars_while(is_space)

    when parse_wildcard(after_spaces) is
        Ok(after_wildcard) -> Ok(([], Str.from_utf8(after_wildcard) |> Result.with_default("")))
        Err(NotWildcard) ->
            (first_comparator, after_first_comparator) =
                parse_comparator(after_spaces)
                |> Result.map_err(|error| InvalidComparator({ index: 0, error }))?

            (other_comparators, after_other_comparators) =
                take_comparators_while_present(after_first_comparator, 1)?

            Ok(
                (
                    List.concat([first_comparator], other_comparators),
                    Str.from_utf8(after_other_comparators) |> Result.with_default(""),
                ),
            )

version_req : Str -> Result VersionReq InvalidVersionReqError
version_req = |s|
    (ver_req, rest) = version_req_lazy(s)?

    if Str.is_empty(rest) then
        Ok(ver_req)
    else
        Err(UnexpectedSuffix(rest))

comparator_lazy : Str -> Result (Comparator, Str) InvalidComparatorError
comparator_lazy = |s|
    (comp, rest) = parse_comparator(Str.to_utf8(s))?

    Ok((comp, Str.from_utf8(rest) |> Result.with_default("")))

comparator : Str -> Result Comparator InvalidComparatorError
comparator = |s|
    (comp, rest) = comparator_lazy(s)?

    if Str.is_empty(rest) then
        Ok(comp)
    else
        Err(UnexpectedSuffix(rest))

parse_comparator : List U8 -> Result (Comparator, List U8) InvalidComparatorError
parse_comparator = |chars|
    when parse_wildcard(chars) is
        Ok(after_wildcard) ->
            Ok((Wildcard(Full), after_wildcard))

        Err(NotWildcard) ->
            (operator, after_operator) = parse_comparator_operator(chars)
            after_spaces = drop_chars_while(after_operator, is_space)

            (major, after_major) =
                parse_numeric_identifier(after_spaces)
                |> Result.map_err(InvalidMajorVersion)?
            (minor, after_minor) =
                parse_comparator_minor_constraint(after_major)?

            when minor is
                NotSpecified -> Ok((Relation({ operator, version: Major(major) }), after_minor))
                Wildcard -> Ok((Wildcard(Major(major)), after_minor))
                Specific(specific_minor) ->
                    (patch, after_patch) =
                        parse_comparator_patch_constraint(after_minor)?

                    when patch is
                        NotSpecified -> Ok((Relation({ operator, version: MajorMinor(major, specific_minor) }), after_patch))
                        Wildcard -> Ok((Wildcard(MajorMinor(major, specific_minor)), after_patch))
                        Specific(specific_patch) ->
                            (pre_release, after_pre_release) =
                                parse_comparator_pre_release(after_patch)?
                            (_build, after_build) =
                                parse_comparator_build(after_pre_release)?

                            Ok(
                                (
                                    Relation(
                                        {
                                            operator,
                                            version: Full(
                                                {
                                                    major,
                                                    minor: specific_minor,
                                                    patch: specific_patch,
                                                    pre_release,
                                                },
                                            ),
                                        },
                                    ),
                                    after_build,
                                ),
                            )

parse_comparator_operator : List U8 -> (ComparatorOperator, List U8)
parse_comparator_operator = |chars|
    when chars is
        [first, .. as rest] if first == '=' ->
            (Exact, rest)

        [first, second, .. as rest] if first == '<' and second == '=' ->
            (LessThanOrEqualTo, rest)

        [first, .. as rest] if first == '<' ->
            (LessThan, rest)

        [first, second, .. as rest] if first == '>' and second == '=' ->
            (GreaterThanOrEqualTo, rest)

        [first, .. as rest] if first == '>' ->
            (GreaterThan, rest)

        [first, .. as rest] if first == '~' ->
            (PatchUpdates, rest)

        [first, .. as rest] if first == '^' ->
            (Compatible, rest)

        _otherwise ->
            (Exact, chars)

parse_comparator_minor_constraint : List U8 -> Result ([Specific U64, Wildcard, NotSpecified], List U8) InvalidComparatorError
parse_comparator_minor_constraint = |chars|
    when parse_byte(chars, '.') is
        Err(ByteNotFound) -> Ok((NotSpecified, chars))
        Ok(after_major_period) ->
            when parse_wildcard(after_major_period) is
                Ok(after_wildcard) -> Ok((Wildcard, after_wildcard))
                Err(NotWildcard) ->
                    (minor, after_minor) =
                        parse_numeric_identifier(after_major_period)
                        |> Result.map_err(InvalidMinorConstraint)?

                    Ok((Specific(minor), after_minor))

parse_comparator_patch_constraint : List U8 -> Result ([Specific U64, Wildcard, NotSpecified], List U8) InvalidComparatorError
parse_comparator_patch_constraint = |chars|
    when parse_byte(chars, '.') is
        Err(ByteNotFound) -> Ok((NotSpecified, chars))
        Ok(after_major_period) ->
            when parse_wildcard(after_major_period) is
                Ok(after_wildcard) -> Ok((Wildcard, after_wildcard))
                Err(NotWildcard) ->
                    (patch, after_patch) =
                        parse_numeric_identifier(after_major_period)
                        |> Result.map_err(InvalidPatchConstraint)?

                    Ok((Specific(patch), after_patch))

parse_comparator_pre_release : List U8 -> Result (List Str, List U8) InvalidComparatorError
parse_comparator_pre_release = |chars|
    when chars is
        [first, .. as rest] if first == '-' ->
            parse_pre_release(rest)
            |> Result.map_err(InvalidPreReleaseConstraint)

        _otherwise -> Ok(([], chars))

parse_comparator_build : List U8 -> Result (List Str, List U8) InvalidComparatorError
parse_comparator_build = |chars|
    when chars is
        [first, .. as rest] if first == '+' ->
            parse_build(rest)
            |> Result.map_err(InvalidBuildConstraint)

        _otherwise -> Ok(([], chars))

parse_alphanumeric_identifier : List U8 -> Result (Str, List U8) [MustHaveNonDigitChars]
parse_alphanumeric_identifier = |chars|
    (digit_chars, chars_after_digits) = take_chars_while(chars, is_digit)
    (non_digit_chars, chars_after_non_digits) = take_chars_while(chars_after_digits, is_non_digit)

    if List.is_empty(non_digit_chars) then
        Err(MustHaveNonDigitChars)
    else
        (other_identifier_chars, rest_of_chars) = take_chars_while(chars_after_non_digits, is_identifier_char)

        alphanumeric_identifier =
            digit_chars
            |> List.concat(non_digit_chars)
            |> List.concat(other_identifier_chars)
            |> Str.from_utf8
            |> Result.with_default("")

        Ok((alphanumeric_identifier, rest_of_chars))

parse_numeric_identifier : List U8 -> Result (U64, List U8) InvalidNumberError
parse_numeric_identifier = |chars|
    take_digits_while_still_numeric = |digit_chars|
        List.walk_until(
            digit_chars,
            (0, digit_chars),
            |(total_num, untaken), char|
                when parse_digit(char) is
                    Err(InvalidDigit) ->
                        Break((total_num, untaken))

                    Ok(num) ->
                        Continue((10 * total_num + num, List.drop_first(untaken, 1))),
        )

    when chars is
        [] ->
            Err(Empty)

        [first] ->
            if is_digit(first) then
                Ok(take_digits_while_still_numeric([first]))
            else
                Err(Empty)

        [first, second, ..] ->
            if is_non_zero_digit(first) then
                Ok(take_digits_while_still_numeric(chars))
            else if first == '0' then
                if is_digit(second) then
                    Err(CannotStartWithZero)
                else
                    Ok((0, chars |> List.drop_first(1)))
            else
                Err(Empty)

parse_byte : List U8, U8 -> Result (List U8) [ByteNotFound]
parse_byte = |chars, byte|
    when chars is
        [first, .. as rest] if first == byte -> Ok(rest)
        _notFound -> Err(ByteNotFound)

parse_wildcard : List U8 -> Result (List U8) [NotWildcard]
parse_wildcard = |chars|
    wildcard_chars = ['*', 'x', 'X']

    when chars is
        [first, .. as rest] if List.contains(wildcard_chars, first) -> Ok(rest)
        _otherwise -> Err(NotWildcard)

parse_digit : U8 -> Result U64 [InvalidDigit]
parse_digit = |ascii_digit|
    if is_digit(ascii_digit) then
        Ok(Num.to_u64((ascii_digit - '0')))
    else
        Err(InvalidDigit)

is_digit : U8 -> Bool
is_digit = |char|
    char >= '0' and char <= '9'

is_non_zero_digit : U8 -> Bool
is_non_zero_digit = |char|
    char >= '1' and char <= '9'

is_letter : U8 -> Bool
is_letter = |char|
    is_lowercase_letter =
        char >= 'a' and char <= 'z'
    is_uppercase_letter =
        char >= 'a' and char <= 'z'

    is_lowercase_letter or is_uppercase_letter

is_non_digit : U8 -> Bool
is_non_digit = |char|
    char == '-' or is_letter(char)

is_identifier_char : U8 -> Bool
is_identifier_char = |char|
    is_non_digit(char) or is_digit(char)

is_space : U8 -> Bool
is_space = |char|
    char == ' '

take_chars_while : List U8, (U8 -> Bool) -> (List U8, List U8)
take_chars_while = |chars, should_take_char|
    List.walk_until(
        chars,
        ([], chars),
        |(taken, untaken), char|
            if should_take_char(char) then
                Continue((List.append(taken, char), List.drop_first(untaken, 1)))
            else
                Break((taken, untaken)),
    )

drop_chars_while : List U8, (U8 -> Bool) -> List U8
drop_chars_while = |chars, should_drop_char|
    number_of_chars_to_drop =
        List.walk_until(
            chars,
            0,
            |dropped_so_far, char|
                if should_drop_char(char) then
                    Continue((dropped_so_far + 1))
                else
                    Break(dropped_so_far),
        )

    chars
    |> List.drop_first(number_of_chars_to_drop)

expect semver("") == Err(EmptySemver)
expect semver("0") == Err(NoPeriodAfterMajorVersion)
expect semver("0.") == Err(InvalidMinorVersion(Empty))
expect semver("0.1") == Err(NoPeriodAfterMinorVersion)
expect semver("0.1.") == Err(InvalidPatchVersion(Empty))

expect
    semver("0.1.0")
    == Ok(
        {
            major: 0,
            minor: 1,
            patch: 0,
            build: [],
            pre_release: [],
        },
    )

expect semver("0.1.0.") == Err(UnexpectedSuffix("."))
expect semver("0.1.00") == Err(InvalidPatchVersion(CannotStartWithZero))

expect
    semver("0.1230.0-alpha.beta")
    == Ok(
        {
            major: 0,
            minor: 1230,
            patch: 0,
            build: [],
            pre_release: ["alpha", "beta"],
        },
    )

expect
    semver("0.1230.0+alpha.beta")
    == Ok(
        {
            major: 0,
            minor: 1230,
            patch: 0,
            build: ["alpha", "beta"],
            pre_release: [],
        },
    )

expect
    semver("0.1230.0-alpha-beta+gamma.delta")
    == Ok(
        {
            major: 0,
            minor: 1230,
            patch: 0,
            build: ["gamma", "delta"],
            pre_release: ["alpha-beta"],
        },
    )

expect
    ## If pre-release segment is after build segment, it's parsed as a build segment
    semver("0.1230.0+alpha-beta")
    == Ok(
        {
            major: 0,
            minor: 1230,
            patch: 0,
            build: ["alpha-beta"],
            pre_release: [],
        },
    )

expect semver("0.1.0-") == Err(InvalidPreRelease(EmptySegment))
expect semver("0.1.0-?") == Err(InvalidPreRelease(InvalidIdentifier))
expect semver("0.1.0+") == Err(InvalidBuild(EmptySegment))
expect semver("0.1.0+?") == Err(InvalidBuild(InvalidIdentifier))

expect
    version_req(">=1.2.3, <1.8.0-alpha.beta")
    == Ok(
        [
            Relation({ operator: GreaterThanOrEqualTo, version: Full({ major: 1, minor: 2, patch: 3, pre_release: [] }) }),
            Relation({ operator: LessThan, version: Full({ major: 1, minor: 8, patch: 0, pre_release: ["alpha", "beta"] }) }),
        ],
    )

expect version_req("") == Err(InvalidComparator({ index: 0, error: InvalidMajorVersion(Empty) }))
expect version_req("*") == Ok([])

expect
    version_req("123")
    == Ok(
        [
            Relation({ operator: Exact, version: Major(123) }),
        ],
    )

expect
    version_req("123.456")
    == Ok(
        [
            Relation({ operator: Exact, version: MajorMinor(123, 456) }),
        ],
    )

expect
    version_req("123.456.789")
    == Ok(
        [
            Relation({ operator: Exact, version: Full({ major: 123, minor: 456, patch: 789, pre_release: [] }) }),
        ],
    )

expect
    version_req("123.456.789-alpha")
    == Ok(
        [
            Relation({ operator: Exact, version: Full({ major: 123, minor: 456, patch: 789, pre_release: ["alpha"] }) }),
        ],
    )

expect
    version_req("123.456.789-alpha+beta")
    == Ok(
        [
            Relation({ operator: Exact, version: Full({ major: 123, minor: 456, patch: 789, pre_release: ["alpha"] }) }),
        ],
    )

expect
    version_req("123.456.789-alpha+")
    == Err(InvalidComparator({ index: 0, error: InvalidBuildConstraint(EmptySegment) }))

expect
    version_req("123.456.789-alpha+?")
    == Err(InvalidComparator({ index: 0, error: InvalidBuildConstraint(InvalidIdentifier) }))

expect
    version_req("1.x")
    == Ok(
        [
            Wildcard(Major(1)),
        ],
    )

expect
    version_req("2.x.x")
    == Err(UnexpectedSuffix(".x"))

expect
    version_req("2.x.3")
    == Err(UnexpectedSuffix(".3"))

expect
    version_req_lazy("4.x ?")
    == Ok(([Wildcard(Major(4))], " ?"))

expect version_req("4.x ?") == Err(UnexpectedSuffix(" ?"))

expect
    "123"
    |> List.repeat(max_comparator_count)
    |> Str.join_with(",")
    |> version_req
    |> Result.map_ok(List.len)
    == Ok(32)

expect
    "123"
    |> List.repeat((max_comparator_count + 1))
    |> Str.join_with(",")
    |> version_req
    == Err(TooManyComparators)

expect
    x = Parse.version_req(">=1.2.2-alpha.beta, <2")
    x
    == Ok(
        [
            Relation({ operator: GreaterThanOrEqualTo, version: Full({ major: 1, minor: 2, patch: 2, pre_release: ["alpha", "beta"] }) }),
            Relation({ operator: LessThan, version: Major(2) }),
        ],
    )

expect
    parsed_operators =
        ["=", ">", ">=", "<", "<=", "~", "^", ""]
        |> List.map_try(
            |op|
                when parse_comparator_operator(Str.to_utf8(op)) is
                    (oper, []) -> Ok(oper)
                    _other -> Err(FailedToParse),
        )

    parsed_operators
    == Ok(
        [
            Exact,
            GreaterThan,
            GreaterThanOrEqualTo,
            LessThan,
            LessThanOrEqualTo,
            PatchUpdates,
            Compatible,
            Exact,
        ],
    )

expect
    Str.to_utf8(".")
    |> parse_comparator_operator
    == (Exact, ['.'])
