roc-semver
==========

A [Roc](https://roc-lang.org) implementation of [semver 2.0.0](https://semver.org/).

Many thanks to the [Rust](https://rust-lang.org) implementation [here](https://github.com/dtolnay/semver)
by David Tolnay.

## Example

```roc
import semver.Parse
import semver.VersionReq

version = Parse.semver "1.2.3-alpha.beta+build--"
verReq = Parse.versionReq ">=1.2, <2"

expect
    version == Ok {
        major: 1,
        minor: 2,
        patch: 3,
        preRelease: ["alpha", "beta"],
        build: ["build--"],
    }

expect
    when (version, verReq) is
        (Ok v, Ok vr) -> VersionReq.matches vr r
        _other -> Bool.false
```

## Roadmap

This library is basically implemented, I just plan to finish up testing
and documentation and then leave it until someone needs a new feature.
