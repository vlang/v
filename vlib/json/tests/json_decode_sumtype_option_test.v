import json

type Any = string | f32 | bool | ?int

fn test_main() {
	x := json.decode([]Any, '["hi", -9.8e7, true, null]')!
	assert dump(x) == [Any('hi'), Any(f32(-9.8e+7)), Any(true), Any('')]
}
