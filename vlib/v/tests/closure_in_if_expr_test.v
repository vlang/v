// Test for closures in if expressions
// Regression test for https://github.com/vlang/v/issues/26595
//
// Bug: inline `if` expression cannot return a closure when
// local variables are declared in the `if` block before the
// closure literal, and the else branch returns an existing
// function-pointer variable.
//
// The fix ensures that function pointer types in if expressions
// are generated correctly, using inline function pointer declarations
// instead of relying on typedefs that may not exist for closure types.
module main

struct Event {
mut:
	value int
}

struct Cfg {
	delay    int
	callback fn (int, mut Event) = unsafe { nil }
}

// Test closure with captured variable in if expression
fn test_closure_in_if_expr_with_capture() {
	cond := true

	result := if cond {
		tag := 'captured'
		fn [tag] (x int, mut e Event) {
			assert tag == 'captured'
		}
	} else {
		fn (x int, mut e Event) {}
	}

	mut e := Event{}
	result(0, mut e)
}

// Test two closures with captures in if expression
fn test_two_closures_in_if_expr() {
	cond1 := true
	cond2 := false

	// Test first branch taken
	result1 := if cond1 {
		tag := 'first'
		fn [tag] (x int, mut e Event) {
			assert tag == 'first'
		}
	} else {
		tag := 'second'
		fn [tag] (x int, mut e Event) {
			assert tag == 'second'
		}
	}

	mut e := Event{}
	result1(0, mut e)

	// Test second branch taken
	result2 := if cond2 {
		tag := 'first'
		fn [tag] (x int, mut e Event) {
			assert tag == 'first'
		}
	} else {
		tag := 'second'
		fn [tag] (x int, mut e Event) {
			assert tag == 'second'
		}
	}

	result2(0, mut e)
}

// Test closure in if expression with function type return
// This is the core pattern from issue #26595:
// - if branch has local declarations before closure literal
// - else branch returns struct field (function pointer)
fn resolve(cfg &Cfg) fn (int, mut Event) {
	result := if cfg.delay > 0 {
		tag := 'tag'
		fn [cfg, tag] (x int, mut e Event) {
			assert tag == 'tag'
			assert cfg.delay == 100
		}
	} else {
		cfg.callback
	}
	return result
}

fn test_closure_with_struct_field_capture() {
	callback := fn (x int, mut e Event) {
		e.value = 42
	}

	cfg := Cfg{
		delay:    100
		callback: callback
	}

	resolved_fn := resolve(&cfg)
	mut e := Event{}
	resolved_fn(0, mut e)

	// Test the else branch
	cfg2 := Cfg{
		delay:    0
		callback: callback
	}
	resolved_fn2 := resolve(&cfg2)
	mut e2 := Event{}
	resolved_fn2(0, mut e2)
	assert e2.value == 42
}

// Test nested if expressions with closures
fn test_nested_if_expr_with_closures() {
	outer_cond := true
	inner_cond := true

	result := if outer_cond {
		tag1 := 'outer'
		if inner_cond {
			tag2 := 'inner'
			fn [tag1, tag2] () string {
				return '${tag1}_${tag2}'
			}
		} else {
			fn [tag1] () string {
				return tag1
			}
		}
	} else {
		fn () string {
			return 'none'
		}
	}

	assert result() == 'outer_inner'
}

// Test closure in if expression with different return types
fn test_closure_returning_value() {
	cond := true

	result := if cond {
		multiplier := 2
		fn [multiplier] (n int) int {
			return n * multiplier
		}
	} else {
		fn (n int) int {
			return n
		}
	}

	assert result(5) == 10
}

// Test the exact pattern from issue #26595
fn test_issue_26595_pattern() {
	struct Event2 {}

	struct Cfg2 {
		delay    int
		callback fn (&int, mut Event2) = unsafe { nil }
	}

	resolve2 := fn (cfg &Cfg2) fn (&int, mut Event2) {
		// This pattern previously caused C compilation error:
		// "error: expected ';' after expression"
		// because the closure type name lacked a typedef
		result := if cfg.delay > 0 {
			tag := 'tag'
			fn [cfg, tag] (x &int, mut e Event2) {
				dump('${tag}')
			}
		} else {
			cfg.callback
		}
		return result
	}

	resolve2(&Cfg2{
		delay:    2000
		callback: fn (x &int, mut e Event2) {}
	})
}

fn main() {
	test_closure_in_if_expr_with_capture()
	test_two_closures_in_if_expr()
	test_closure_with_struct_field_capture()
	test_nested_if_expr_with_closures()
	test_closure_returning_value()
	test_issue_26595_pattern()
	println('All tests passed!')
}
