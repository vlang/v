fn a() chan string {
	ch_out := chan string{}
	f := fn (a chan string) {
		a <- 'foo'
	}
	spawn f(ch_out)
	return ch_out
}

fn b(ch_in chan string) string {
	f := fn (a chan string, b chan string) {
		val := <-a
		{}
		b <- val
	}
	ch_out := chan string{}
	spawn f(ch_in, ch_out)
	return <-ch_out
}

fn test_main() {
	ch0 := a()
	assert b(ch0) == 'foo'
}
