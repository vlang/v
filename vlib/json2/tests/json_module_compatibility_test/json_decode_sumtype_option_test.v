// vtest vflags: -w
import json2

// Not named `Any`: a main sum type that shares its name with `json2.Any` is still
// decoded as `json2.Any` inside `json2.decode[[]T]`.
type Value = string | f32 | bool | ?int

fn test_main() {
	x := json2.decode[[]Value]('["hi", -9.8e7, true, null]')!
	assert dump(x) == [Value('hi'), Value(f32(-9.8e+7)), Value(true), Value(?int(none))]
}
