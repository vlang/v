struct Struct {
	ch chan int
}

fn listen(ch chan int) int {
	value := <-ch
	if _ := <-ch {
		assert false, 'expected the channel to close after one value'
	}
	return value
}

fn test_main() {
	mut array := [3]chan int{}
	mut listeners := []thread int{cap: array.len}
	for a in 0 .. array.len {
		listeners << spawn listen(array[a])
	}
	for i, a in array {
		a <- i
	}
	for c in array {
		c.close()
	}
	assert listeners.wait() == [0, 1, 2]
}
