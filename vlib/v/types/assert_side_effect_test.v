module types

import os

// A `-prod` build removes assert statements with their conditions, so a channel receive, an
// assignment, an increment or a `wait()` inside an assert does not happen there (the shape that hung
// vlib/sync/park_test.v). Those are reported in every build. A call that modifies its receiver, or
// takes a `mut` argument, is reported only for `-prod`: many methods take a `mut` receiver to read.
const assert_side_effect_source = "
fn bump(mut a []int) bool {
	a << 1
	return true
}

fn main() {
	ch := chan int{cap: 2}
	ch <- 1
	assert <-ch == 1
	mut values := [1, 2, 3]
	assert values.pop() == 3
	assert bump(mut values)
	assert values.len == 3 && values.contains(1)
	mut m := {'a': 1}
	assert m.delete('a') == none
	assert 'text'.contains('x')
}
"

// The operand of `typeof` and `sizeof` is read for its type and never evaluated: on this empty
// unbuffered channel a receive would block for ever, and the program runs to its end. Binding the
// receive first, as the notice advises, is what would hang it.
const assert_unevaluated_operand_source = "
fn main() {
	ch := chan int{}
	assert typeof(<-ch).name == 'int'
	assert sizeof(<-ch) == sizeof(int)
	mut n := 0
	assert typeof(n++).name == 'int'
	assert n == 0
}
"

fn assert_side_effect_notices(flags []string) []string {
	return assert_notices_of(assert_side_effect_source, flags)
}

fn assert_notices_of(program string, flags []string) []string {
	root := os.join_path(os.vtmp_dir(), 'assert_side_effect_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, program.trim_left('\n')) or { panic(err) }
	result := os.exec([@VEXE, ...flags, '-check', source])
	assert result.exit_code == 0, result.output
	return result.output.split_into_lines().filter(it.contains('notice: assert contains'))
}

fn test_assert_side_effects_that_always_happen_are_reported_in_every_build() {
	notices := assert_side_effect_notices([])
	assert notices.len == 1, notices.join('\n')
	assert notices[0].contains('main.v:9:9') || notices[0].contains(':9:9:'), notices[0]
	assert notices[0].contains('a channel receive')
}

fn test_assert_calls_that_modify_are_reported_for_prod() {
	notices := assert_side_effect_notices(['-prod'])
	assert notices.len == 4, notices.join('\n')
	assert notices[0].contains('a channel receive')
	assert notices[1].contains('a call that modifies its receiver')
	assert notices[2].contains('a call with a `mut` argument')
	assert notices[3].contains('a call that modifies its receiver')
}

fn test_an_operand_read_only_for_its_type_is_not_reported() {
	for flags in [[]string{}, ['-prod']] {
		notices := assert_notices_of(assert_unevaluated_operand_source, flags)
		assert notices.len == 0, '${flags}: ' + notices.join('\n')
	}
}
