module util

import strings

// Surrounder is an utility to help you manage a stack of additions, that
// should be done *both* _before_ and _after_ a given piece of generated
// code, in a synchronised way. It does so by forcing you to put the
// creation/destruction samples next to each other, so that it is easier to
// keep them in sync and spot errors.
// Note: Surrounder writes the creation samples (`befores`) in the _same_ order
// they were added, and the destruction samples (`afters`) in _reverse_ order.
//
// Usage:
// ```v
// mut sr := new_surrounder(2) // just a rough initial guess; it can grow
// sr.add('string tmp1 = ...;', 'string_free(&tmp1);')
// sr.add('string tmp2 = ...;', 'string_free(&tmp2);')
// ..
// sr.builder_write_befores(mut some_string_builder)
// some_string_builder.writeln('MIDDLE_that_uses_tmp1_and_tmp2')
// sr.builder_write_afters(mut some_string_builder)
// ```
// ... will produce this in `some_string_builder`:
// ```
// string tmp1 = ...;
// string tmp2 = ...;
// MIDDLE_that_uses_tmp1_and_tmp2
// string_free(&tmp2);
// string_free(&tmp1);
// ```

[noinit]
pub struct Surrounder {
mut:
	befores []string
	afters  []string
}

// new_surrounder creates a new Surrounder instance.
// The expected_length is a hint for the initial capacity for the
// befores/afters stacks.
pub fn new_surrounder(expected_length int) Surrounder {
	return Surrounder{
		befores: []string{cap: expected_length}
		afters: []string{cap: expected_length}
	}
}

// add appends a `before`/`after` pair to the surrounder
// When you call .after() or .builder_write_afters(),
// all `before` parts will be in order, while all `after`
// parts, will be in reverse order.
pub fn (mut s Surrounder) add(before string, after string) {
	s.befores << before
	s.afters << after
}

// before returns all the `before` parts that were accumulated so far
[manualfree]
pub fn (s &Surrounder) before() string {
	len := s.befores.len
	if len > 0 {
		mut res := strings.new_builder(len * 100)
		defer {
			unsafe { res.free() }
		}
		for i := 0; i < len; i++ {
			x := s.befores[i]
			if x.len > 0 {
				res.writeln(x)
			}
		}
		ret := res.str()
		return ret
	}
	return ''
}

// after returns all the `after` parts that were accumulated so far,
// in reverse order of their addition.
[manualfree]
pub fn (s &Surrounder) after() string {
	len := s.afters.len
	if len > 0 {
		mut res := strings.new_builder(len * 100)
		defer {
			unsafe { res.free() }
		}
		for i := len - 1; i >= 0; i-- {
			x := s.afters[i]
			if x.len > 0 {
				res.writeln(x)
			}
		}
		ret := res.str()
		return ret
	}
	return ''
}

// builder_write_befores writeln-es all the `before` parts into the given
// string builder `sb`.
pub fn (s &Surrounder) builder_write_befores(mut sb strings.Builder) {
	len := s.befores.len
	if len > 0 {
		for i := 0; i < len; i++ {
			x := s.befores[i]
			if x.len > 0 {
				sb.writeln(x)
			}
		}
	}
}

// builder_write_afters writeln-es all the `after` parts into the given
// string builder `sb`. They will be written there in reverse order, compared
// to how/when they were added.
pub fn (s &Surrounder) builder_write_afters(mut sb strings.Builder) {
	len := s.afters.len
	if len > 0 {
		for i := len - 1; i >= 0; i-- {
			x := s.afters[i]
			if x.len > 0 {
				sb.writeln(x)
			}
		}
	}
}

// free frees the private resources associated with the surrounder instance
// Called automatically by `-autofree`, or in `[manualfree]` tagged functions.
[unsafe]
pub fn (mut s Surrounder) free() {
	unsafe {
		s.befores.free()
		s.afters.free()
	}
}
