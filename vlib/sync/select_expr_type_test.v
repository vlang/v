import time

// `select` used as an expression is a `bool`: it prints and interpolates as one
fn test_select_expression_is_a_bool() {
	q := chan int{cap: 1}
	q <- 7
	open := select {
		v := <-q {
			assert v == 7
		}
		100 * time.millisecond {
		}
	}
	assert open
	assert '${open}' == 'true'
	q.close()
	closed_ok := select {
		_ := <-q {
		}
		20 * time.millisecond {
		}
	}
	assert !closed_ok
	assert closed_ok.str() == 'false'
	assert '${closed_ok}' == 'false'
}
