import sync

struct One {
pub mut:
	i int
}

fn (mut o One) add(i int) {
	o.i = o.i + i
}

fn run(mut once sync.Once, mut o One, c chan bool) {
	once.do(fn [mut o] () {
		o.add(5)
	})
	c <- true
}

fn test_once() {
	mut o := &One{}
	mut once := sync.new_once()
	c := chan bool{}
	n := 10

	// It is executed 10 times, but only once actually.
	for i := 0; i < n; i++ {
		spawn run(mut once, mut o, c)
	}
	for i := 0; i < n; i++ {
		<-c
	}
	assert o.i == 5
}
