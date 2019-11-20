import rand
import time

fn main() {
	rand.seed(time.now().uni)

	for _ in 0..10 {
		println('${rand.next(255)}.${rand.next(255)}.${rand.next(255)}.${rand.next(255)}')
	}
}
