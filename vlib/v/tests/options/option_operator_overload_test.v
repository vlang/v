import time

struct SubTestTimeOptional[T] {
	iis string
	ext T
}

pub fn (s1 SubTestTimeOptional[T]) == (s2 SubTestTimeOptional[T]) bool {
	return s1.iis == s2.iis
}

struct TestTimeOptional {
	exp ?time.Time
}

fn now_optional[T]() SubTestTimeOptional[T] {
	return SubTestTimeOptional[TestTimeOptional]{
		iis: 'Vtest'
		ext: TestTimeOptional{
			exp: time.now()
		}
	}
}

fn now_delay_optional[T]() SubTestTimeOptional[T] {
	return SubTestTimeOptional[T]{
		iis: 'Vtest'
		ext: TestTimeOptional{
			exp: time.now().add_seconds(5)
		}
	}
}

fn test_main() {
	mut t1 := now_optional[TestTimeOptional]()
	mut t2 := now_delay_optional[TestTimeOptional]()
	assert t1 != t2
}
