module main

struct Log {
mut:
	order   int
	message string
}

@[noinline]
fn leaf(mut log Log) {
	mut digit := 1
	defer { log.order = log.order * 10 + digit }
	digit = 2
	panic('bare panic')
}

@[noinline]
fn middle(mut log Log) {
	defer { log.order = log.order * 10 + 3 }
	leaf(mut log)
}

@[noinline]
fn capture(mut log Log) {
	defer { log.order = log.order * 10 + 5 }
	defer {
		if message := recover() {
			log.message = message
			log.order = log.order * 10 + 4
		}
	}
	middle(mut log)
}

fn main() {
	// Running twice also verifies that the first recovery unlinks every frame.
	for _ in 0 .. 2 {
		mut log := Log{}
		capture(mut log)
		if log.order != 2345 || log.message != 'bare panic' {
			exit(1)
		}
	}
}
