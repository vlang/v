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
