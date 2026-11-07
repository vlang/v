fn main() {
	ch := chan int{cap: 1}
	ch <- 42
	v := <-ch
	println(v)
}
