## Description

`semver` is a library for processing versions, that use the [semver][semver] format.

## Examples

```v
import semver

fn main() {
	ver1 := semver.from('1.2.4') or {
		println('Invalid version')
		return
	}
	ver2 := semver.from('2.3.4') or {
		println('Invalid version')
		return
	}
	println(ver1 > ver2)
	println(ver2 > ver1)
	println(ver1.satisfies('>=1.1.0 <2.0.0'))
	println(ver2.satisfies('>=1.1.0 <2.0.0'))
	println(ver2.satisfies('>=1.1.0 <2.0.0 || >2.2.0'))
}
```

```
false
true
true
false
true
```

For more details see `semver.v` file.

Tilde ranges allow changes to the patch when a minor is specified: `~2.0` and
`~2.0.0` mean `>=2.0.0 <2.1.0`. A bare major such as `~2` allows minor changes
up to `3.0.0`.

Comparison operators accept wildcard or missing version components. For example,
`>=1.x` means `>=1.0.0`, `>1.x` means `>=2.0.0`, and `<=1.2.x` stops before
`1.3.0`, including its prereleases. Wildcard or partial comparators can be combined
with other comparators in any order, as in `>=1.x <2.0.0` or `>=0.0.0 >1.2`.
Wildcard components must be whole `x`, `X`, or `*` tokens. Prerelease tags on
wildcard ranges do not change their release floor; for example, `>=1.2.x-beta.1`
starts at `1.2.0` and does not admit `1.2.0-beta.1`.

[semver]: https://semver.org/

Malformed comparator sets return a descriptive parse error when range expansion fails.

Each whitespace-separated operand in a comparator set is evaluated and intersected with the
others. Partial versions and wildcards describe a series: `1.2` and `1.2.x` cover all stable
versions from `1.2.0` up to `1.3.0`. Operators apply to those series, so `>=1.2` includes
`1.2.0`, `<=1.2` includes `1.2.9`, and `>1.2` starts at `1.3.0`. A wildcard major with `=`,
`>=`, or `<=` accepts every stable version; `<*` and `>*` accept none.

Prereleases require an explicit prerelease comparator with the same major, minor, and patch
in that set. Advanced ranges keep their exclusive ceiling at the next series' `-0` boundary,
so adding a prerelease comparator cannot admit the next series. Hyphens inside prerelease
identifiers and wildcard letters in build metadata are literal characters.

## Reporting malformed ranges

`Version.satisfies(range)` returns `false` when a range cannot be parsed.
Use `Version.satisfies_or_error(range)` to distinguish an invalid range from a valid
range that does not match. `semver.is_valid_range(range)` checks the range syntax
without choosing a version. An empty range is valid and matches every release version.
It follows the normal prerelease exclusion rule described above.

```v
import semver

fn main() {
	version := semver.from('1.2.3')!
	assert version.satisfies_or_error('^1.0.0')!
	assert !version.satisfies_or_error('^2.0.0')!
	assert semver.is_valid_range('')
	assert !semver.is_valid_range('not-a-version')
}
```
