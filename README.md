roc-semver
==========

A [Roc](https://roc-lang.org) implementation of [semver 2.0.0](https://semver.org/).

Many thanks to the [Rust](https://rust-lang.org) implementation [here](https://github.com/dtolnay/semver)
by David Tolnay.

## Example

```roc
import semver.Semver
import semver.VersionReq

versionRes = Semver.parse "1.2.3-alpha.beta+build--"
versionReqRes = VersionReq.parse ">=1.2.3-alpha.beta, <2"

expect
    versionRes == Ok {
        major: 1,
        minor: 2,
        patch: 3,
        pre_release: ["alpha", "beta"],
        build: ["build--"],
    }

expect
    when (versionRes, versionReqRes) is
        (Ok version, Ok versionReq) ->
            versionReq |> VersionReq.matches version

        _other -> Bool.false
```
