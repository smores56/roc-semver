module [
    semver,
    semverLazy,
    comparator,
    comparatorLazy,
    versionReq,
    versionReqLazy,
]

import AsciiCode
import Types exposing [
    Semver,
    VersionReq,
    Comparator,
]
import Error exposing [
    InvalidSemverError,
    InvalidVersionReqError,
    InvalidComparatorError,
    InvalidNumberError,
    InvalidIdentifierError,
]

maxComparatorCount = 32

semverLazy : Str -> Result { version : Semver, rest : Str } InvalidSemverError
semverLazy = \s ->
    {} <- assertSemverNotEmpty s
        |> Result.try

    bytes = Str.toUtf8 s

    (major, afterMajor) <- parseNumericIdentifier bytes
        |> Result.mapErr InvalidMajorVersion
        |> Result.try
    afterMajorDot <- parseByte afterMajor AsciiCode.period
        |> Result.mapErr \ByteNotFound -> NoPeriodAfterMajorVersion
        |> Result.try

    (minor, afterMinor) <- parseNumericIdentifier afterMajorDot
        |> Result.mapErr InvalidMinorVersion
        |> Result.try
    afterMinorDot <- parseByte afterMinor AsciiCode.period
        |> Result.mapErr \ByteNotFound -> NoPeriodAfterMinorVersion
        |> Result.try

    (patch, afterPatch) <- parseNumericIdentifier afterMinorDot
        |> Result.mapErr InvalidPatchVersion
        |> Result.try

    (preRelease, afterPreRelease) <- parseOptionalPreRelease afterPatch
        |> Result.mapErr InvalidPreRelease
        |> Result.try
    (build, afterBuild) <- parseOptionalBuild afterPreRelease
        |> Result.mapErr InvalidBuild
        |> Result.try

    Ok {
        version: {
            major,
            minor,
            patch,
            build,
            preRelease,
        },
        rest: afterBuild
        |> Str.fromUtf8
        |> Result.withDefault "",
    }

semver : Str -> Result Semver InvalidSemverError
semver = \s ->
    { version, rest } <- semverLazy s
        |> Result.try

    if Str.isEmpty rest then
        Ok version
    else
        Err (UnexpectedSuffix rest)

assertSemverNotEmpty : Str -> Result {} [EmptySemver]
assertSemverNotEmpty = \s ->
    if Str.isEmpty s then
        Err EmptySemver
    else
        Ok {}

parseOptionalPreRelease : List U8 -> Result (List Str, List U8) InvalidIdentifierError
parseOptionalPreRelease = \chars ->
    when chars is
        [first, .. as rest] if first == AsciiCode.hyphen -> parsePreRelease rest
        _other -> Ok ([], chars)

parsePreRelease : List U8 -> Result (List Str, List U8) InvalidIdentifierError
parsePreRelease = \chars ->
    parsePreReleaseIdentifier = \idChars ->
        when parseAlphanumericIdentifier idChars is
            Ok (identifier, rest) -> Ok (identifier, rest)
            Err MustHaveNonDigitChars ->
                when parseNumericIdentifier idChars is
                    Ok (num, rest) -> Ok (Num.toStr num, rest)
                    Err err ->
                        when err is
                            Empty ->
                                Err EmptySegment

                            CannotStartWithZero | InvalidNumber ->
                                Err InvalidIdentifier

    takeRemainingPreReleaseIdentifiers = \tailChars ->
        when tailChars is
            [first, .. as rest] if first == AsciiCode.period ->
                (identifier, remaining) <- parsePreReleaseIdentifier rest
                    |> Result.try
                (foundIdentifiers, leftover) <- takeRemainingPreReleaseIdentifiers remaining
                    |> Result.try

                Ok (List.concat [identifier] foundIdentifiers, leftover)

            _other -> Ok ([], tailChars)

    when parsePreReleaseIdentifier chars is
        Ok (firstIdentifier, afterFirstIdentifier) ->
            (remainingIdentifiers, rest) <- takeRemainingPreReleaseIdentifiers afterFirstIdentifier
                |> Result.try

            Ok (List.concat [firstIdentifier] remainingIdentifiers, rest)

        Err err ->
            if err == EmptySegment && !(List.isEmpty chars) then
                Err InvalidIdentifier
            else
                Err err

parseOptionalBuild : List U8 -> Result (List Str, List U8) InvalidIdentifierError
parseOptionalBuild = \chars ->
    when chars is
        [first, .. as rest] if first == AsciiCode.plus -> parseBuild rest
        _other -> Ok ([], chars)

parseBuild : List U8 -> Result (List Str, List U8) InvalidIdentifierError
parseBuild = \chars ->
    parseBuildIdentifier = \idChars ->
        when parseAlphanumericIdentifier idChars is
            Ok (identifier, rest) -> Ok (identifier, rest)
            Err MustHaveNonDigitChars ->
                (digits, leftover) = takeCharsWhile idChars isDigit
                if List.isEmpty digits then
                    Err EmptySegment
                else
                    Ok (digits |> Str.fromUtf8 |> Result.withDefault "", leftover)

    takeRemainingBuildIdentifiers = \tailChars ->
        when tailChars is
            [first, .. as rest] if first == AsciiCode.period ->
                (identifier, remaining) <- parseBuildIdentifier rest
                    |> Result.try
                (foundIdentifiers, leftover) <- takeRemainingBuildIdentifiers remaining
                    |> Result.try

                Ok (List.concat [identifier] foundIdentifiers, leftover)

            _other -> Ok ([], tailChars)

    when parseBuildIdentifier chars is
        Ok (firstIdentifier, afterFirstIdentifier) ->
            (remainingIdentifiers, rest) <- takeRemainingBuildIdentifiers afterFirstIdentifier
                |> Result.try

            Ok (List.concat [firstIdentifier] remainingIdentifiers, rest)

        Err err ->
            if err == EmptySegment && !(List.isEmpty chars) then
                Err InvalidIdentifier
            else
                Err err

versionReqLazy : Str -> Result (VersionReq, Str) InvalidVersionReqError
versionReqLazy = \s ->
    takeComparatorsWhilePresent = \tailChars, reqIndex ->
        when tailChars is
            [first, .. as rest] if first == AsciiCode.comma ->
                if reqIndex >= maxComparatorCount then
                    Err TooManyComparators
                else
                    tailAfterSpaces = dropCharsWhile rest isSpace
                    (comp, afterComp) <- parseComparator tailAfterSpaces
                        |> Result.mapErr \error -> InvalidComparator { index: reqIndex, error }
                        |> Result.try
                    (comps, afterComps) <- takeComparatorsWhilePresent afterComp (reqIndex + 1)
                        |> Result.try

                    Ok (List.concat [comp] comps, afterComps)

            _otherwise -> Ok ([], tailChars)

    afterSpaces =
        Str.toUtf8 s
        |> dropCharsWhile isSpace

    when parseWildcard afterSpaces is
        Ok afterWildcard -> Ok ([], Str.fromUtf8 afterWildcard |> Result.withDefault "")
        Err NotWildcard ->
            (firstComparator, afterFirstComparator) <- parseComparator afterSpaces
                |> Result.mapErr \error -> InvalidComparator { index: 0, error }
                |> Result.try

            (otherComparators, afterOtherComparators) <- takeComparatorsWhilePresent afterFirstComparator 1
                |> Result.try

            Ok (
                List.concat [firstComparator] otherComparators,
                Str.fromUtf8 afterOtherComparators |> Result.withDefault "",
            )

versionReq : Str -> Result VersionReq InvalidVersionReqError
versionReq = \s ->
    (verReq, rest) <- versionReqLazy s
        |> Result.try

    if Str.isEmpty rest then
        Ok verReq
    else
        Err (UnexpectedSuffix rest)

comparatorLazy : Str -> Result (Comparator, Str) InvalidComparatorError
comparatorLazy = \s ->
    (comp, rest) <- parseComparator (Str.toUtf8 s)
        |> Result.try

    Ok (comp, Str.fromUtf8 rest |> Result.withDefault "")

comparator : Str -> Result Comparator InvalidComparatorError
comparator = \s ->
    (comp, rest) <- comparatorLazy s
        |> Result.try

    if Str.isEmpty rest then
        Ok comp
    else
        Err (UnexpectedSuffix rest)

parseComparator : List U8 -> Result (Comparator, List U8) InvalidComparatorError
parseComparator = \chars ->
    (maybeOperator, afterOperator) = parseComparisonOperator chars
    afterSpaces = dropCharsWhile afterOperator isSpace

    (major, afterMajor) <- parseNumericIdentifier afterSpaces
        |> Result.mapErr InvalidMajorVersion
        |> Result.try

    { minor, afterMinor, minorHasWildcard } <- parseComparatorMinorConstraint afterMajor
        |> Result.try
    { patch, afterPatch, patchHasWildcard } <- parseComparatorPatchConstraint { chars: afterMinor, minorHasWildcard }
        |> Result.try

    # TODO: move to helper function
    preReleaseResult =
        if Result.isOk patch then
            when afterPatch is
                [first, .. as rest] if first == AsciiCode.hyphen ->
                    parsePreRelease rest
                    |> Result.mapErr InvalidPreReleaseConstraint

                _otherwise -> Ok ([], afterPatch)
        else
            Ok ([], afterPatch)

    (preRelease, afterPreRelease) <- preReleaseResult
        |> Result.try

    # TODO: move to helper function
    buildResult =
        if Result.isOk patch then
            when afterPreRelease is
                [first, .. as rest] if first == AsciiCode.plus ->
                    parseBuild rest
                    |> Result.mapErr InvalidBuildConstraint

                _otherwise -> Ok ([], afterPreRelease)
        else
            Ok ([], afterPreRelease)

    (_build, afterBuild) <- buildResult
        |> Result.try

    operator =
        when maybeOperator is
            Ok op -> op
            Err NoOperator ->
                if minorHasWildcard || patchHasWildcard then
                    Wildcard
                else
                    Compatible

    Ok (
        {
            operator,
            major,
            minor,
            patch,
            preRelease,
        },
        afterBuild,
    )

parseComparatorMinorConstraint : List U8
    -> Result
        {
            minor : Result U64 [NotSpecified],
            afterMinor : List U8,
            minorHasWildcard : Bool,
        }
        InvalidComparatorError
parseComparatorMinorConstraint = \chars ->
    when parseByte chars AsciiCode.period is
        Err ByteNotFound ->
            Ok {
                minor: Err NotSpecified,
                afterMinor: chars,
                minorHasWildcard: Bool.false,
            }

        Ok afterMajorPeriod ->
            when parseWildcard afterMajorPeriod is
                Ok afterWildcard ->
                    Ok {
                        minor: Err NotSpecified,
                        afterMinor: afterWildcard,
                        minorHasWildcard: Bool.true,
                    }

                Err NotWildcard ->
                    (minor, afterMinor) <- parseNumericIdentifier afterMajorPeriod
                        |> Result.mapErr InvalidMinorConstraint
                        |> Result.try

                    Ok {
                        minor: Ok minor,
                        afterMinor,
                        minorHasWildcard: Bool.false,
                    }

parseComparatorPatchConstraint : { chars : List U8, minorHasWildcard : Bool }
    -> Result
        {
            patch : Result U64 [NotSpecified],
            afterPatch : List U8,
            patchHasWildcard : Bool,
        }
        InvalidComparatorError
parseComparatorPatchConstraint = \{ chars, minorHasWildcard } ->
    when parseByte chars AsciiCode.period is
        Err ByteNotFound ->
            Ok {
                patch: Err NotSpecified,
                afterPatch: chars,
                patchHasWildcard: Bool.false,
            }

        Ok afterMajorPeriod ->
            when parseWildcard afterMajorPeriod is
                Ok afterWildcard ->
                    Ok {
                        patch: Err NotSpecified,
                        afterPatch: afterWildcard,
                        patchHasWildcard: Bool.true,
                    }

                Err NotWildcard ->
                    if minorHasWildcard then
                        Err PatchSpecifiedAfterMinorWildcard
                    else
                        (patch, afterPatch) <- parseNumericIdentifier afterMajorPeriod
                            |> Result.mapErr InvalidPatchConstraint
                            |> Result.try

                        Ok {
                            patch: Ok patch,
                            afterPatch,
                            patchHasWildcard: Bool.false,
                        }

parseComparisonOperator : List U8 -> (Result ComparisonOperator [NoOperator], List U8)
parseComparisonOperator = \chars ->
    when chars is
        [first, .. as rest] if first == AsciiCode.equals ->
            (Ok Exact, rest)

        [first, second, .. as rest] if first == AsciiCode.lessThan && second == AsciiCode.equals ->
            (Ok LessThanOrEqualTo, rest)

        [first, .. as rest] if first == AsciiCode.lessThan ->
            (Ok LessThan, rest)

        [first, second, .. as rest] if first == AsciiCode.greaterThan && second == AsciiCode.equals ->
            (Ok GreaterThanOrEqualTo, rest)

        [first, .. as rest] if first == AsciiCode.greaterThan ->
            (Ok GreaterThan, rest)

        [first, .. as rest] if first == AsciiCode.tilde ->
            (Ok PatchUpdates, rest)

        [first, .. as rest] if first == AsciiCode.caret ->
            (Ok Compatible, rest)

        _otherwise ->
            (Err NoOperator, chars)

parseAlphanumericIdentifier : List U8 -> Result (Str, List U8) [MustHaveNonDigitChars]
parseAlphanumericIdentifier = \chars ->
    (digitChars, charsAfterDigits) = takeCharsWhile chars isDigit
    (nonDigitChars, charsAfterNonDigits) = takeCharsWhile charsAfterDigits isNonDigit

    if List.isEmpty nonDigitChars then
        Err MustHaveNonDigitChars
    else
        (otherIdentifierChars, restOfChars) = takeCharsWhile charsAfterNonDigits isIdentifierChar

        alphanumericIdentifier =
            digitChars
            |> List.concat nonDigitChars
            |> List.concat otherIdentifierChars
            |> Str.fromUtf8
            |> Result.withDefault ""

        Ok (alphanumericIdentifier, restOfChars)

parseNumericIdentifier : List U8 -> Result (U64, List U8) InvalidNumberError
parseNumericIdentifier = \chars ->
    takeDigitsWhileStillNumeric = \digitChars ->
        List.walkUntil digitChars (0, digitChars) \(totalNum, untaken), char ->
            when parseDigit char is
                Err InvalidDigit ->
                    Break (totalNum, untaken)

                Ok num ->
                    Continue (10 * totalNum + num, List.dropFirst untaken 1)

    when chars is
        [] ->
            Err Empty

        [first] ->
            if isDigit first then
                Ok (0, [])
            else
                Err Empty

        [first, second, ..] ->
            if isNonZeroDigit first then
                Ok (takeDigitsWhileStillNumeric chars)
            else if first == AsciiCode.zero then
                if isDigit second then
                    Err CannotStartWithZero
                else
                    Ok (0, chars |> List.dropFirst 1)
            else
                Err Empty

parseByte : List U8, U8 -> Result (List U8) [ByteNotFound]
parseByte = \chars, byte ->
    when chars is
        [first, .. as rest] if first == byte -> Ok rest
        _notFound -> Err ByteNotFound

parseWildcard : List U8 -> Result (List U8) [NotWildcard]
parseWildcard = \chars ->
    wildcardChars = [AsciiCode.star, AsciiCode.lowerX, AsciiCode.upperX]

    when chars is
        [first, .. as rest] if List.contains wildcardChars first -> Ok rest
        _otherwise -> Err NotWildcard

parseDigit : U8 -> Result U64 [InvalidDigit]
parseDigit = \asciiDigit ->
    if isDigit asciiDigit then
        Ok (Num.toU64 (asciiDigit - AsciiCode.zero))
    else
        Err InvalidDigit

isDigit : U8 -> Bool
isDigit = \char ->
    char >= AsciiCode.zero && char <= AsciiCode.nine

isNonZeroDigit : U8 -> Bool
isNonZeroDigit = \char ->
    char >= AsciiCode.one && char <= AsciiCode.nine

isLetter : U8 -> Bool
isLetter = \char ->
    isLowercaseLetter =
        char >= AsciiCode.lowerA && char <= AsciiCode.lowerZ
    isUppercaseLetter =
        char >= AsciiCode.upperA && char <= AsciiCode.upperZ

    isLowercaseLetter || isUppercaseLetter

isNonDigit : U8 -> Bool
isNonDigit = \char ->
    char == AsciiCode.hyphen || isLetter char

isIdentifierChar : U8 -> Bool
isIdentifierChar = \char ->
    isNonDigit char || isDigit char

isSpace : U8 -> Bool
isSpace = \char ->
    char == AsciiCode.space

takeCharsWhile : List U8, (U8 -> Bool) -> (List U8, List U8)
takeCharsWhile = \chars, shouldTakeChar ->
    List.walkUntil chars ([], chars) \(taken, untaken), char ->
        if shouldTakeChar char then
            Continue (List.append taken char, List.dropFirst untaken 1)
        else
            Break (taken, untaken)

dropCharsWhile : List U8, (U8 -> Bool) -> List U8
dropCharsWhile = \chars, shouldDropChar ->
    numberOfCharsToDrop =
        List.walkUntil chars 0 \droppedSoFar, char ->
            if shouldDropChar char then
                Continue (droppedSoFar + 1)
            else
                Break droppedSoFar

    chars
    |> List.dropFirst numberOfCharsToDrop

expect semver "" == Err EmptySemver
expect semver "0" == Err NoPeriodAfterMajorVersion
expect semver "0." == Err (InvalidMinorVersion Empty)
expect semver "0.1" == Err NoPeriodAfterMinorVersion
expect semver "0.1." == Err (InvalidPatchVersion Empty)

expect
    semver "0.1.0"
    == Ok {
        major: 0,
        minor: 1,
        patch: 0,
        build: [],
        preRelease: [],
    }

expect semver "0.1.0." == Err (UnexpectedSuffix ".")
expect semver "0.1.00" == Err (InvalidPatchVersion CannotStartWithZero)

expect
    semver "0.1230.0-alpha.beta"
    == Ok {
        major: 0,
        minor: 1230,
        patch: 0,
        build: [],
        preRelease: ["alpha", "beta"],
    }

expect
    semver "0.1230.0+alpha.beta"
    == Ok {
        major: 0,
        minor: 1230,
        patch: 0,
        build: ["alpha", "beta"],
        preRelease: [],
    }

expect
    semver "0.1230.0-alpha-beta+gamma.delta"
    == Ok {
        major: 0,
        minor: 1230,
        patch: 0,
        build: ["gamma", "delta"],
        preRelease: ["alpha-beta"],
    }

expect
    ## If pre-release segment is after build segment, it's parsed as a build segment
    semver "0.1230.0+alpha-beta"
    == Ok {
        major: 0,
        minor: 1230,
        patch: 0,
        build: ["alpha-beta"],
        preRelease: [],
    }

expect semver "0.1.0-" == Err (InvalidPreRelease EmptySegment)
expect semver "0.1.0-?" == Err (InvalidPreRelease InvalidIdentifier)
expect semver "0.1.0+" == Err (InvalidBuild EmptySegment)
expect semver "0.1.0+?" == Err (InvalidBuild InvalidIdentifier)

expect
    versionReq ">=1.2.3, <1.8.0-alpha.beta"
    == Ok [
        { operator: GreaterThanOrEqualTo, major: 1, minor: Ok 2, patch: Ok 3, preRelease: [] },
        { operator: LessThan, major: 1, minor: Ok 8, patch: Ok 0, preRelease: ["alpha", "beta"] },
    ]

expect versionReq "" == Err (InvalidComparator { index: 0, error: InvalidMajorVersion Empty })
expect versionReq "*" == Ok []

expect
    versionReq "123"
    == Ok [
        { operator: Compatible, major: 123, minor: Err NotSpecified, patch: Err NotSpecified, preRelease: [] },
    ]

expect
    versionReq "123.456"
    == Ok [
        { operator: Compatible, major: 123, minor: Ok 456, patch: Err NotSpecified, preRelease: [] },
    ]

expect
    versionReq "123.456.789"
    == Ok [
        { operator: Compatible, major: 123, minor: Ok 456, patch: Ok 789, preRelease: [] },
    ]

expect
    versionReq "123.456.789-alpha"
    == Ok [
        { operator: Compatible, major: 123, minor: Ok 456, patch: Ok 789, preRelease: ["alpha"] },
    ]

expect
    versionReq "123.456.789-alpha+beta"
    == Ok [
        { operator: Compatible, major: 123, minor: Ok 456, patch: Ok 789, preRelease: ["alpha"] },
    ]

expect
    versionReq "123.456.789-alpha+"
    == Err (InvalidComparator { index: 0, error: InvalidBuildConstraint EmptySegment })

expect
    versionReq "123.456.789-alpha+?"
    == Err (InvalidComparator { index: 0, error: InvalidBuildConstraint InvalidIdentifier })

expect
    versionReq "1.x"
    == Ok [
        { operator: Wildcard, major: 1, minor: Err NotSpecified, patch: Err NotSpecified, preRelease: [] },
    ]

expect
    versionReq "2.x.x"
    == Ok [
        { operator: Wildcard, major: 2, minor: Err NotSpecified, patch: Err NotSpecified, preRelease: [] },
    ]

expect
    versionReq "2.x.3"
    == Err (InvalidComparator { index: 0, error: PatchSpecifiedAfterMinorWildcard })

expect
    versionReqLazy "4.x ?"
    == Ok (
        [{ operator: Wildcard, major: 4, minor: Err NotSpecified, patch: Err NotSpecified, preRelease: [] }],
        " ?",
    )

expect versionReq "4.x ?" == Err (UnexpectedSuffix " ?")

expect
    "123"
    |> List.repeat maxComparatorCount
    |> Str.joinWith ","
    |> versionReq
    |> Result.map List.len
    == Ok 32

expect
    "123"
    |> List.repeat (maxComparatorCount + 1)
    |> Str.joinWith ","
    |> versionReq
    == Err TooManyComparators

expect
    parsedOperators =
        ["=", ">", ">=", "<", "<=", "~", "^"]
        |> List.mapTry \op ->
            when parseComparisonOperator (Str.toUtf8 op) is
                (Ok oper, []) -> Ok oper
                _other -> Err FailedToParse

    parsedOperators
    == Ok [
        Exact,
        GreaterThan,
        GreaterThanOrEqualTo,
        LessThan,
        LessThanOrEqualTo,
        PatchUpdates,
        Compatible,
    ]

expect
    Str.toUtf8 "."
    |> parseComparisonOperator
    == (Err NoOperator, [AsciiCode.period])
