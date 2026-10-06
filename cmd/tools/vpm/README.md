# V package manager

Install a package from VPM with:

```sh
v install ui2
```

Git packages include their file contents in the initial clone, so checkout can complete
without a second network request for missing file blobs. Submodules are installed recursively.

## Semantic version ranges

Explicit ranges select the highest matching semantic-version Git tag:

```sh
v install 'vsl@^0.1.47'
v install 'nedpals.args@>=0.4.0 <0.6.0'
v install 'markdown@*'
```

The same dependency strings can appear in `v.mod`. Ranges support the syntax
provided by `semver`, including caret, tilde, comparators, wildcards, hyphen
ranges and `||`. Tags may have an optional lowercase `v` prefix. Non-version
tags are ignored; prereleases require a matching prerelease constraint.
Version components must fit the `int` fields of `semver.Version`.
If no tag satisfies the range, installation fails.

Exact Git refs such as `@v1.2.3`, `@v1.2.3-rc.x`, `@1.2.3+build.x`, or `@topic.x`
keep their existing meaning. An implicit `x` or `X` wildcard must occur in the numeric
version core, as in `1.x` or `1.2.X`, rather than a prerelease or build identifier.
A bare module name continues to install its default branch.

Project installs record the original range in `v.mod.lock` together with the
selected tag and commit. An unchanged range reuses the locked commit, including
with `--locked`, even when newer tags exist. A changed range is resolved again;
`--locked` rejects changes or a locked tag outside the constraint.

This is the initial range-selection layer. Different requirements targeting the
same installation directory are rejected when a range is involved, before
installation changes the module store. Joint constraint solving, backtracking,
version-aware updates and graph reporting are not yet implemented.
