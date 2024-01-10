import sync

struct Counter {
pub mut:
	i int
}

fn (mut c Counter) add(i int) {
	c.i = c.i + i
}

fn run(mut m sync.ManyTimes, mut co Counter, c chan bool) {
	m.do(fn [mut co] () {
		co.add(5)
	})
	c <- true
}

fn test_many_times_once() {
	mut co := &Counter{}
	mut m := sync.new_many_times(1)
	c := chan bool{}
	n := 10

	// It is executed 10 times, but only once actually.
	for i := 0; i < n; i++ {
		spawn run(mut m, mut co, c)
	}
	for i := 0; i < n; i++ {
		<-c
	}
	assert co.i == 5
}

fn test_many_times_fifth() {
	mut co := &Counter{}
	mut m := sync.new_many_times(5)
	c := chan bool{}
	n := 10

	// It is executed 10 times, but only 5 times actually.
	for i := 0; i < n; i++ {
		spawn run(mut m, mut co, c)
	}
	for i := 0; i < n; i++ {
		<-c
	}
	assert co.i == 25
}
