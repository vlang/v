type Result = Success | Failure

struct Success {
	value int
}

struct Failure {
	msg string
}

fn process(r Result) Result {
	return match r {
		Success {
			if r.value > 10 {
				Success{
					value: r.value * 2
				}
			} else {
				r
			}
		}
		Failure {
			r
		}
	}
}

fn test_autofree_match_with_nested_if() {
	r := process(Success{
		value: 15
	})
	assert r is Success
	assert (r as Success).value == 30
}

fn test_autofree_match_with_nested_if_else() {
	r := process(Success{
		value: 5
	})
	assert r is Success
	assert (r as Success).value == 5
}

fn test_autofree_match_error_branch() {
	r := process(Failure{
		msg: 'test error'
	})
	assert r is Failure
	assert (r as Failure).msg == 'test error'
}
