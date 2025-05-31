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

[semver]: https://semver.org/
