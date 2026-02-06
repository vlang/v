struct Foo[T] {
	val T
}

type Bar[T] = fn (T) bool

fn t[T](val T) {
	_ := Bar[T](fn [T](a T) bool {
		return true
	})
}

fn test_main() {
	t[int](1)
}
