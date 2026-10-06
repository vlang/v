# V package manager

Install a package from VPM with:

```sh
v install ui2
```

Git packages include their file contents in the initial clone, so checkout can complete
without a second network request for missing file blobs. Submodules are installed recursively.

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
The lockfile format is unchanged.

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
`--latest` widens selected direct dependencies to the newest resolvable stable release, writes
caret constraints back to every selected direct requirement in `v.mod`, including URL aliases,
and records them in the lockfile. Transitive requirements still
apply. Rewriting `v.mod` retains its fields but uses the manifest encoder's formatting.
These options require a project. Projects without ranges retain branch-based update behavior.

## Inspecting versions and requirements

`v why PACKAGE` shows the constraints declared by each parent and the installed version.
Unversioned trees retain their existing display. `v mod graph` prints one flat edge per dependency,
including versions and constraints, and marks missing packages. Both commands work offline:

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
checkouts nor the lockfile. Outside constrained projects, the existing branch-based report remains.

Additional manifest fields and coexisting major versions are separate phases of the proposal.
