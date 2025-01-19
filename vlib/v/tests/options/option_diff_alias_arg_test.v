import time

struct Foo[T] {
pub:
	value ?T
}

fn t(x ?i64) i64 {
	return x or { -1 }
}

fn test_main() {
	bar := Foo[time.Duration]{
		value: time.Duration(0)
	}

	assert t(bar.value) == 0
}
