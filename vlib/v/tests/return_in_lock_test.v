import time

struct AA {
mut:
	b string
}

fn test_return_lock() {
	start := time.now()
	shared s := AA{'3'}
	reader(shared s)
	lock s {
		assert s.b == '5'
		s.b = '4'
	}
	rlock s {
		assert s.b == '4'
	}
}

fn reader(shared s AA) {
	lock s {
		assert s.b == '3'
		s.b = '5'
		// this test checks if cgen unlocks the mutex here
		return
	}
}

fn test_multi_return_lock() {
	shared s := AA{'3'}
	reti, retb := printer2(shared s)
	lock s {
		assert s.b == '3'
		assert reti == 4
		assert retb == true
	}
}

fn printer2(shared s AA) (int, bool) {
	rlock s {
		assert s.b == '3'
		return 4, true
	}
	return 5, false
}
