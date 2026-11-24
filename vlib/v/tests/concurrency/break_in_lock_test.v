import time

struct AA {
mut:
	b string
}

fn test_return_lock() {
	shared s := AA{'3'}
	done := chan string{cap: 1}
	spawn fn (shared s AA, done chan string) {
		reader(shared s)
		verifier := spawn read_value(shared s)
		done <- verifier.wait()
	}(shared s, done)
	select {
		value := <-done {
			assert value == '5'
		}
		2 * time.second {
			assert false, 'another thread could not acquire the lock after break'
		}
	}
}

fn read_value(shared s AA) string {
	return lock s {
		s.b
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
