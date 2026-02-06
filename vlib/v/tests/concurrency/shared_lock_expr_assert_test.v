struct Counter {
mut:
	value int
}

fn (shared c Counter) inc() {
	lock c {
		c.value += 1
	}
}

fn (c Counter) val() int {
	return c.value
}

fn test_main() {
	shared c := Counter{1}

	assert rlock c {
		c.val()
	} == 1
	if rlock c {
		c.val()
	} == 1 {
		assert true
	}

	c.inc()

	assert rlock c {
		c.val()
	} == 2
	if rlock c {
		c.val()
	} == 2 {
		assert true
	}
}
