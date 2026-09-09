// https://github.com/vlang/v/issues/27789
struct Bar {}

struct Baz {}

struct Gen[T] {}

fn foo[T]() int {
	mut r := 0
	$match T {
		$int {
			r = 1
		}
		$float {
			r = 2
		}
		Bar {
			r = 3
		}
		$else {
			r = -1
		}
	}
	return r
}

fn test_comptime_match_mixing_comptime_and_normal_types() {
	assert foo[int]() == 1
	assert foo[f32]() == 2
	assert foo[Bar]() == 3
	assert foo[[]u8]() == -1
}

fn multi[T]() int {
	$match T {
		Bar, Baz {
			return 3
		}
		Gen[int] {
			return 4
		}
		$int {
			return 1
		}
		$else {
			return -1
		}
	}
	return 0
}

fn test_comptime_match_multi_type_branches() {
	assert multi[int]() == 1
	assert multi[Bar]() == 3
	assert multi[Baz]() == 3
	assert multi[Gen[int]]() == 4
	assert multi[string]() == -1
}
