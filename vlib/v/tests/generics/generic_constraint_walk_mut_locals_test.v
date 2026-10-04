import strings

// A program that declares a constraint has the bodies of its generic functions
// walked for what the constraints forbid. Resolving a chained call there,
// `c.bump().value()`, checks `c.bump()`, which must not report a `mut` or a
// `shared` local `c` as immutable, however the body declares it.
type Number = int | f64

struct Counter {
mut:
	n int
}

fn (mut c Counter) bump() Counter {
	c.n++
	return c
}

fn (c Counter) value() int {
	return c.n
}

fn counter_pair(n int) (Counter, Counter) {
	return Counter{
		n: n
	}, Counter{
		n: n + 10
	}
}

fn maybe_counter(n int) ?Counter {
	if n < 0 {
		return none
	}
	return Counter{
		n: n
	}
}

fn double[T Number](x T) T {
	return x + x
}

fn render[T](x T) string {
	mut sb := strings.new_builder(16)
	sb.write_string(' ${x} ')
	return sb.str().trim_space()
}

fn bump_local[T](start T) int {
	mut c := Counter{
		n: int(start)
	}
	return c.bump().value()
}

fn bump_parallel[T](start T) int {
	mut a, mut b := Counter{
		n: int(start)
	}, Counter{
		n: int(start) + 10
	}
	return a.bump().value() + b.bump().value()
}

fn bump_multi_return[T](start T) int {
	mut a, mut b := counter_pair(int(start))
	return a.bump().value() + b.bump().value()
}

fn bump_guard[T](start T) int {
	if mut c := maybe_counter(int(start)) {
		return c.bump().value()
	}
	return -1
}

fn bump_or_block[T](start T) int {
	mut c := maybe_counter(-1) or {
		Counter{
			n: int(start)
		}
	}
	return c.bump().value()
}

fn bump_loop[T](start T) int {
	mut counters := [Counter{
		n: int(start)
	}, Counter{
		n: int(start) + 10
	}]
	mut sum := 0
	for mut c in counters {
		sum += c.bump().value()
	}
	return sum + counters[0].value()
}

fn bump_closure_param[T](start T) int {
	bump := fn (mut c Counter) int {
		return c.bump().value()
	}
	mut c := Counter{
		n: int(start)
	}
	return bump(mut c)
}

fn bump_locked[T](start T) int {
	shared c := Counter{
		n: int(start)
	}
	lock c {
		return c.bump().value()
	}
	return -1
}

fn test_mut_locals_in_generic_bodies_of_a_program_with_constraints() {
	assert double(21) == 42
	assert render(42) == '42'
	assert bump_local(1) == 2
	assert bump_parallel(1) == 14
	assert bump_multi_return(1) == 14
	assert bump_guard(1) == 2
	assert bump_or_block(1) == 2
	assert bump_loop(1) == 16
	assert bump_closure_param(1) == 2
	assert bump_locked(1) == 2
}
