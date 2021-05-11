import time

struct AA {
mut:
	b string
}

const (
	sleep_time = time.millisecond * 50
)

fn test_return_lock() {
	shared s := AA{'3'}
	go printer(shared s)
	go fn (shared s AA) {
		start := time.now()
		for {
			reader(shared s)
			if time.now() - start > sleep_time {
				exit(0)
			}
		}
	}(shared s)
	time.sleep(sleep_time * 2)
	assert false
}

fn printer(shared s AA) {
	start := time.now()
	for {
		lock s {
			assert s.b in ['0', '1', '2', '3', '4', '5']
		}
		if time.now() - start > time.millisecond * 50 {
			exit(0)
		}
	}
}

fn reader(shared s AA) {
	mut i := 0
	for {
		i++
		x := i.str()
		lock s {
			s.b = x
			if s.b == '5' {
				// this test checks if cgen unlocks the mutex here
				break
			}
		}
	}
}
