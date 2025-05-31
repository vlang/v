module main

fn test_main() {
	my := MyError{
		path: 'err msg'
	}
	p := my.to[string](|m| m.path)
	p2 := my.to_str(|m| m.path)
	println(p)
	println(p2)

	assert p == p2
}

struct MyError {
pub:
	path string
}

fn (e &MyError) msg() string {
	return e.path
}

fn (e &MyError) code() int {
	return 1
}

fn (e &MyError) to[T](func fn (MyError) T) T {
	return func(e)
}

fn (e &MyError) to_str(func fn (MyError) string) string {
	return func(e)
}
