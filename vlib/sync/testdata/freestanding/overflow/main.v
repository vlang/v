module main

import sync

fn main() {
	mut sem := sync.new_semaphore_init(0xffffffff)
	sem.post()
	println('overflow was accepted')
}
