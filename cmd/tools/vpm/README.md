# V package manager

Install a package from VPM with:

```sh
v install ui2
```

Git packages include their file contents in the initial clone, so checkout can complete
without a second network request for missing file blobs. Submodules are installed recursively.
Git installs use shallow clones. Branch updates follow their configured upstream using a
fast-forward pull; detached tag or lockfile checkouts outside constrained projects follow the
origin's default branch. Updates refuse uncommitted changes or unpublished local commits.
Exact project refs stay pinned along with their lockfile entries.

## Semantic version ranges

Explicit ranges select semantic-version Git tags satisfying the whole dependency graph:

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

Requirements for the same repository are solved together. VPM tries higher matching tags first,
then backtracks to older releases when their dependencies conflict. A bare requirement can share
another dependency's tagged selection or exact Git ref, regardless of requirement order.
Bare-only installs still use the default branch, and exact Git refs remain pins.
Repository URL aliases share one selection; different repositories cannot
replace each other at the same normalized installation path. Candidate checkouts are staged in
temporary directories, and the complete graph is resolved before any installed checkout changes.
An unsatisfiable graph reports both requirement chains and leaves installed modules and the project
lockfile untouched.

The lockfile is preferred before newer releases. `--locked` restricts resolution to recorded
commits; `--frozen` also prevents writing the lockfile.
Complete project resolution drops lock entries for dependencies no longer reachable.
The lockfile format remains version 1, with an optional package-content SHA256 hash.
Independent clones exclude VCS metadata and hash relative paths, file bytes and symlink targets.
Hash mismatches stop locked installation before any installed checkout changes.

## Updating constrained projects

In a project using ranges, `v update` resolves the complete dependency graph again inside its
existing constraints, then installs the selected releases and refreshes the lockfile. It does
not move a range installation to the repository's default branch. A targeted update prefers
other locked releases, but can adjust them when the updated package changes its dependencies.
Checkouts with local Git changes or unpublished commits are still protected.

```sh
v update
v update -p vsl
v update -p vsl --precise v0.1.45
v update --latest
```

`--precise` selects one package version or commit and fails if it violates any requirement.
Numeric versions can match a tag with the optional `v` prefix.
Targeted updates accept any direct repository alias, including one listed only in
`dev_dependencies`. Aliases of the same repository share the targeted selection, and
`--precise` must still satisfy every regular and development requirement on that repository.
`--latest` widens selected direct dependencies to the newest resolvable stable release, writes
caret constraints back to every selected direct requirement in `v.mod`, including URL aliases,
and records them in the lockfile. Transitive requirements still
apply. Rewriting `v.mod` retains its fields but uses the manifest encoder's formatting.
These options require a project. Projects without ranges retain branch-based update behavior.
`--dry-run` resolves and reports proposed versions without changing installed checkouts,
`v.mod`, or `v.mod.lock`. For branch updates it reads the origin HEAD without fetching into
the installed repository.

## Inspecting versions and requirements

`v why PACKAGE` shows the constraints declared by each parent and the installed version.
Unversioned trees retain their existing display. `v mod graph` prints one flat edge per dependency,
including versions and constraints, and marks missing packages. `v mod graph --imports`
shows the source import tree instead, with nested imports indented and modules shown once.
These commands work offline:

```text
myapp -> vsl@v0.1.47 (requires ^0.1.47)
```

For projects using ranges, `v outdated` prints these columns for installed dependencies:

- **Current**: the installed tag or locked revision.
- **Upgradable**: the newest tag admitted by the currently installed graph's requirements.
- **Resolvable**: the version selected when the complete graph is resolved again, including the
  candidate releases' own dependency manifests.
- **Latest**: the newest stable semantic-version tag, ignoring project constraints.

`-` means no tagged version is available. Unlike `Current`, the other columns inspect remote
release tags; `Resolvable` also reads candidate manifests. This command changes neither installed
checkouts nor the lockfile. Outside constrained projects, `v outdated` reports all installed
packages using the same columns;
`Resolvable` there considers direct root requirements when available. Commit-based upgrade checks
retain their previous behavior.

## Root metadata, release policy and vendoring

Project commands include the root manifest's `dev_dependencies` alongside `dependencies`.
Vendoring includes development requirements and their runtime dependencies. Outdated reporting
checks every regular and development requirement on a package, including exact version pins.
`v install` resolves and locks both sets together; `v update`, `v outdated`, `v why`,
`v mod graph`, and `v vendor` use the same requirements. Development requirements declared by
dependency modules are excluded. `v update --latest` widens a development requirement in
`dev_dependencies`, keeping it separate from regular requirements in `v.mod`.

Only the root manifest supplies `dependency_overrides`. Global `package: ref` selectors apply
throughout the graph; `parent>package: ref` selectors apply on that parent's requiring edge.
A conditional `parent@range>package: version` selector applies when the override version
satisfies `range`; it does not select by the parent's own version.
A matching scoped selector takes precedence. Use `-` as the selected ref to remove an edge.
Selected manifests are checked against their `min_v` before their dependencies are resolved.

Ranged selections accept `--exclude-newer` as RFC3339 or `YYYY-MM-DD` at midnight UTC,
and `--minimum-release-age` as hours or a duration with a `d`, `h` or `m` suffix.
Tags are dated by their commit timestamp. Invalid policy and failed discovery are errors.
Exact refs and matching locked revisions keep their explicit meaning.

A release may declare `retracted: ['1.2.3', '>=2.0.0 <2.0.2']` in its `v.mod`.
Automatic version selection and `v outdated` read these ranges from the latest stable
release and exclude matching tags. Invalid retraction ranges are errors. Existing locked
revisions, exact pins, and `--precise` selections remain reproducible, even when retracted.

`v vendor` copies the complete installed dependency graph into `vendor/` at the nearest
project root. Publication uses a complete staged copy; missing modules and existing
vendor destinations cause an error. Set `VMODULES` to the project's absolute `vendor` path
when compiling from that copy.

Manifest `catalog: { package: 'range' }` and `workspaces: ['path/*']` metadata are
available through `v.vmod.Manifest` and preserved by `vmod.encode`. Catalog keys may
be quoted. Duplicate keys, malformed values and non-string workspaces are errors.
These fields store metadata without expanding dependency aliases.
Registry-qualified dependency keys remain distinct in lock data.

The candidate search behind `resolve_with_pubgrub` is test-only: it delegates to the consistent
backtracking search, which the test suite exercises. Dependency constraints are checked in both
directions; a complete conflict-driven PubGrub algorithm is pending.
