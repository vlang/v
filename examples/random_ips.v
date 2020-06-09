import rand
import time

fn main() {
	rand.seed(int(time.now().unix))

	for _ in 0..10 {
		println('${rand.intn(255)}.${rand.intn(255)}.${rand.intn(255)}.${rand.intn(255)}')
	}
}
