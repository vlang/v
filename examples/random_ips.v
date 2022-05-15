import rand

fn main() {
	for _ in 0 .. 10 {
		println('${rand.intn(255)?}.${rand.intn(255)?}.${rand.intn(255)?}.${rand.intn(255)?}')
	}
}
