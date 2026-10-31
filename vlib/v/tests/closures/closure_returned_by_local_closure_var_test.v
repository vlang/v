// A closure stored in a local variable whose result is itself a closure: the call on the
// result used to be emitted as a call on an integer, because the result type was read as the
// closure's own result type.
fn test_call_on_the_closure_returned_by_a_local_closure() {
	acc := fn (a int) fn (int) int {
		return fn [a] (b int) int {
			return a + b
		}
	}
	assert acc(3)(4) == 7
	add3 := acc(3)
	assert add3(10) == 13
	assert typeof(acc).name == 'fn (int) fn (int) int'
}

fn test_outer_closure_that_captures_a_variable() {
	base := 10
	scale := fn [base] (a int) fn (int) int {
		return fn [a, base] (b int) int {
			return base + a * b
		}
	}
	assert scale(3)(4) == 22
	times2 := scale(2)
	assert times2(5) == 20
}

fn test_returned_closure_without_captures() {
	twice := fn (a int) fn (int) int {
		return fn (b int) int {
			return b * 2
		}
	}
	assert twice(3)(4) == 8
}

fn test_closure_returning_a_closure_that_returns_a_closure() {
	curry := fn (a int) fn (int) fn (int) int {
		return fn [a] (b int) fn (int) int {
			return fn [a, b] (c int) int {
				return a * 100 + b * 10 + c
			}
		}
	}
	assert curry(1)(2)(3) == 123
}

fn test_direct_call_of_a_closure_literal_returning_a_closure() {
	assert fn (a int) fn (int) int {
		return fn [a] (b int) int {
			return a - b
		}
	}(9)(4) == 5
}
