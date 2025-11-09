struct Data {
mut:
	counter int
}

fn (mut d Data) operation(fail bool) !int {
	if fail {
		return error('')
	}
	return 10
}

fn (mut d Data) run_operation(fail bool) !int {
	return d.operation(fail) or {
		defer {
			d.counter++
		}
		err
	}
}

fn test_defer_with_or_expr() {
	mut d := Data{}
	d.run_operation(true) or {}
	assert d.counter == 1

	d.run_operation(false) or {}
	assert d.counter == 1

	d.run_operation(true) or {}
	assert d.counter == 2
}

fn some() !int {
	return 5
}

fn test_if_expr_with_defer() {
	mut abc := 0
	x := if v := some() {
		defer { assert abc == 9090 }
		defer { abc = 9090 }
		v
	} else {
		2004
	}
	assert x == 5
}

fn test_scoped_defer() {
	mut res := 0

	defer {
		res++
		assert res == 5
	}
	{
		res++
		defer {
			res++
			assert res == 4
		}
		{
			res++
			defer {
				res++
				assert res == 3
			}
		} // <- Block 2 ends. Defer 3 executes. res = 3.
	} // <- Block 1 ends. Defer 2 executes. res = 4.
} // <- 'test_scoped_defer' ends. Defer 1 executes. res = 5.

fn test_defer_with_comptime_if() {
	mut c := 0
	defer { assert c == 3 }
	defer { c++ }
	$if tinyc || gcc || clang || msvc || mingw {
		defer { c++ }
		c++
	} $else {
		c = 0
	}
}

fn test_defer_with_comptime_match() {
	mut c := 0
	defer { assert c == 3 }
	defer { c++ }
	$match @CCOMPILER {
		'tinyc', 'gcc', 'clang', 'msvc', 'mingw' {
			defer { c++ }
			c++
		}
		$else {
			c = 0
		}
	}
}

fn test_defer_with_comptime_for() {
	mut c := 0
	$for f in Data.fields {
		defer {
			if f.name == 'counter' {
				c++
			}
		}
	}
	assert c == 1

	$for m in Data.methods {
		defer {
			if m.name in ['operation', 'run_operation'] {
				c++
			}
		}
	}
	assert c == 3
}

fn test_defer_fn_with_inner_var() {
	mut x := 0
	defer {
		assert x == 1
	}
	{
		a := 1
		defer(fn) {
			x = a
		}
	}
}
