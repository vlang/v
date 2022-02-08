import sync

// NB: this is the same test as `vlib/sync/once_test.v`, but
// it uses an explicit passing of the voidptr parameter in
// once.do_with_param/2, instead of passing a closure of it
// in once.do/1.
// Closures are not yet implemented on Windows.

struct One {
pub mut:
	i int
}

fn (mut o One) add(i int) {
	o.i = o.i + i
}

fn run(mut once sync.Once, mut o One, c chan bool) {
	once.do_with_param(fn (mut o One) {
		o.add(5)
	}, o)
	c <- true
}

fn test_once() {
	mut o := &One{}
	mut once := sync.new_once()
	c := chan bool{}
	n := 10

	// It is executed 10 times, but only once actually.
	for i := 0; i < n; i++ {
		go run(mut once, mut o, c)
	}
	for i := 0; i < n; i++ {
		<-c
	}
	assert o.i == 5
}
