type Any = []Any | f64 | int | map[string]Any | string

struct Arr {
	ints   []int
	floats []f64
	strs   []string
}

fn encode[T](typ T) map[string]Any {
	mut mp := map[string]Any{}
	$for field in T.fields {
		value := typ.$(field.name)
		$if field.is_enum {
			mp[field.name] = Any(int(value))
		} $else $if field.is_array {
			mp[field.name] = value.map(Any(it))
		} $else {
			mp[field.name] = Any(value)
		}
	}
	return mp
}

fn test_main() {
	a := Arr{[5], [2.0], ['asdf']}
	r := encode[Arr](a)
	assert unsafe { r['ints'] } == Any([Any(5)])
	assert unsafe { r['floats'] } == Any([Any(2.0)])
	assert unsafe { r['strs'] } == Any([Any('asdf')])
}
