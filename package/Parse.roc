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
    ComparatorOperator,
]
import Error exposing [
    InvalidSemverError,
    InvalidVersionReqError,
    InvalidComparatorError,
    InvalidNumberError,
    InvalidIdentifierError,
]

maxComparatorCount = 32

semverLazy : Str -> Result (Semver, Str) InvalidSemverError
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

    Ok (
        {
            major,
            minor,
            patch,
            build,
            preRelease,
        },
        afterBuild
        |> Str.fromUtf8
        |> Result.withDefault "",
    )

semver : Str -> Result Semver InvalidSemverError
semver = \s ->
    (version, rest) <- semverLazy s
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
    when parseWildcard chars is
        Ok afterWildcard ->
            Ok (Wildcard Full, afterWildcard)

        Err NotWildcard ->
            (operator, afterOperator) = parseComparatorOperator chars
            afterSpaces = dropCharsWhile afterOperator isSpace

            (major, afterMajor) <- parseNumericIdentifier afterSpaces
                |> Result.mapErr InvalidMajorVersion
                |> Result.try
            (minor, afterMinor) <- parseComparatorMinorConstraint afterMajor
                |> Result.try

            when minor is
                NotSpecified -> Ok (Relation { operator, version: Major major }, afterMinor)
                Wildcard -> Ok (Wildcard (Major major), afterMinor)
                Specific specificMinor ->
                    (patch, afterPatch) <- parseComparatorPatchConstraint afterMinor
                        |> Result.try

                    when patch is
                        NotSpecified -> Ok (Relation { operator, version: MajorMinor major specificMinor }, afterPatch)
                        Wildcard -> Ok (Wildcard (MajorMinor major specificMinor), afterPatch)
                        Specific specificPatch ->
                            (preRelease, afterPreRelease) <- parseComparatorPreRelease afterPatch
                                |> Result.try
                            (_build, afterBuild) <- parseComparatorBuild afterPreRelease
                                |> Result.try

                            Ok (
                                Relation {
                                    operator,
                                    version: Full {
                                        major,
                                        minor: specificMinor,
                                        patch: specificPatch,
                                        preRelease,
                                    },
                                },
                                afterBuild,
                            )

parseComparatorOperator : List U8 -> (ComparatorOperator, List U8)
parseComparatorOperator = \chars ->
    when chars is
        [first, .. as rest] if first == AsciiCode.equals ->
            (Exact, rest)

        [first, second, .. as rest] if first == AsciiCode.lessThan && second == AsciiCode.equals ->
            (LessThanOrEqualTo, rest)

        [first, .. as rest] if first == AsciiCode.lessThan ->
            (LessThan, rest)

        [first, second, .. as rest] if first == AsciiCode.greaterThan && second == AsciiCode.equals ->
            (GreaterThanOrEqualTo, rest)

        [first, .. as rest] if first == AsciiCode.greaterThan ->
            (GreaterThan, rest)

        [first, .. as rest] if first == AsciiCode.tilde ->
            (PatchUpdates, rest)

        [first, .. as rest] if first == AsciiCode.caret ->
            (Compatible, rest)

        _otherwise ->
            (Exact, chars)

parseComparatorMinorConstraint : List U8 -> Result ([Specific U64, Wildcard, NotSpecified], List U8) InvalidComparatorError
parseComparatorMinorConstraint = \chars ->
    when parseByte chars AsciiCode.period is
        Err ByteNotFound -> Ok (NotSpecified, chars)
        Ok afterMajorPeriod ->
            when parseWildcard afterMajorPeriod is
                Ok afterWildcard -> Ok (Wildcard, afterWildcard)
                Err NotWildcard ->
                    (minor, afterMinor) <- parseNumericIdentifier afterMajorPeriod
                        |> Result.mapErr InvalidMinorConstraint
                        |> Result.try

                    Ok (Specific minor, afterMinor)

parseComparatorPatchConstraint : List U8 -> Result ([Specific U64, Wildcard, NotSpecified], List U8) InvalidComparatorError
parseComparatorPatchConstraint = \chars ->
    when parseByte chars AsciiCode.period is
        Err ByteNotFound -> Ok (NotSpecified, chars)
        Ok afterMajorPeriod ->
            when parseWildcard afterMajorPeriod is
                Ok afterWildcard -> Ok (Wildcard, afterWildcard)
                Err NotWildcard ->
                    (patch, afterPatch) <- parseNumericIdentifier afterMajorPeriod
                        |> Result.mapErr InvalidPatchConstraint
                        |> Result.try

                    Ok (Specific patch, afterPatch)

parseComparatorPreRelease : List U8 -> Result (List Str, List U8) InvalidComparatorError
parseComparatorPreRelease = \chars ->
    when chars is
        [first, .. as rest] if first == AsciiCode.hyphen ->
            parsePreRelease rest
            |> Result.mapErr InvalidPreReleaseConstraint

        _otherwise -> Ok ([], chars)

parseComparatorBuild : List U8 -> Result (List Str, List U8) InvalidComparatorError
parseComparatorBuild = \chars ->
    when chars is
        [first, .. as rest] if first == AsciiCode.plus ->
            parseBuild rest
            |> Result.mapErr InvalidBuildConstraint

        _otherwise -> Ok ([], chars)

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
                Ok (takeDigitsWhileStillNumeric [first])
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
        Relation { operator: GreaterThanOrEqualTo, version: Full { major: 1, minor: 2, patch: 3, preRelease: [] } },
        Relation { operator: LessThan, version: Full { major: 1, minor: 8, patch: 0, preRelease: ["alpha", "beta"] } },
    ]

expect versionReq "" == Err (InvalidComparator { index: 0, error: InvalidMajorVersion Empty })
expect versionReq "*" == Ok []

expect
    versionReq "123"
    == Ok [
        Relation { operator: Exact, version: Major 123 },
    ]

expect
    versionReq "123.456"
    == Ok [
        Relation { operator: Exact, version: MajorMinor 123 456 },
    ]

expect
    versionReq "123.456.789"
    == Ok [
        Relation { operator: Exact, version: Full { major: 123, minor: 456, patch: 789, preRelease: [] } },
    ]

expect
    versionReq "123.456.789-alpha"
    == Ok [
        Relation { operator: Exact, version: Full { major: 123, minor: 456, patch: 789, preRelease: ["alpha"] } },
    ]

expect
    versionReq "123.456.789-alpha+beta"
    == Ok [
        Relation { operator: Exact, version: Full { major: 123, minor: 456, patch: 789, preRelease: ["alpha"] } },
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
        Wildcard (Major 1),
    ]

expect
    versionReq "2.x.x"
    == Err (UnexpectedSuffix ".x")

expect
    versionReq "2.x.3"
    == Err (UnexpectedSuffix ".3")

expect
    versionReqLazy "4.x ?"
    == Ok ([Wildcard (Major 4)], " ?")

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
    x = Parse.versionReq ">=1.2.2-alpha.beta, <2"
    x
    == Ok [
        Relation { operator: GreaterThanOrEqualTo, version: Full { major: 1, minor: 2, patch: 2, preRelease: ["alpha", "beta"] } },
        Relation { operator: LessThan, version: Major 2 },
    ]

expect
    parsedOperators =
        ["=", ">", ">=", "<", "<=", "~", "^", ""]
        |> List.mapTry \op ->
            when parseComparatorOperator (Str.toUtf8 op) is
                (oper, []) -> Ok oper
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
        Exact,
    ]

expect
    Str.toUtf8 "."
    |> parseComparatorOperator
    == (Exact, [AsciiCode.period])
