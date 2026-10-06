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

## Minimum compiler versions and root overrides

A dependency can declare `min_v: '0.5.0'` in `v.mod`. Installation checks this
against the running compiler before consuming that dependency's own dependencies.
An empty requirement is allowed; an invalid version or an unmet requirement fails
installation for both registered modules and direct repositories.

The root project's manifest can force a dependency ref or semantic version range:

```text
Module {
    dependencies: ['publisher.package@^1.0.0']
    dependency_overrides: ['publisher.package: v2.0.0']
}
```

Only the root manifest supplies overrides. Dependency manifests cannot override
the consumer's choices. Overrides match a registered name, or a direct repository's
basename or manifest name, and replace its requested constraint before its selected
manifest and dependencies are read. A direct repository with a different manifest
name may require a default-branch metadata checkout to discover that name.
Malformed or duplicate overrides fail before installation.

The lockfile records the effective dependency request including the override,
the selected tag and the actual commit. Reinstalling reuses that commit. Changing
an override requires a normal install to refresh the lock; `--locked` rejects it.
An override can intentionally select a version outside the original constraint.

Bundled tools declare external build requirements in their own `v.mod` under
`dev_dependencies`, for example `dev_dependencies: ['markdown']` for `vdoc`.
The launcher reads these manifests beside its own compiler. The legacy
`v.util.external_modules_for_tool` function remains available as a wrapper;
`external_module_dependencies_for_tool` remains the old compatibility snapshot.
