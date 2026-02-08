struct Opt[T] {
	val T
pub:
	some bool
}

fn some[T](val T) Opt[T] {
	return Opt[T]{
		val:  val
		some: true
	}
}

fn (f Opt[T]) map[U](op fn (T) U) Opt[U] {
	if f.some {
		return some[U](op(f.val))
	}
	return Opt[U]{}
}

fn test_main() {
	f := some('hello')
	result := f.map(fn (s string) int {
		return s.len
	})
	assert result.some && result.val == 5
}
