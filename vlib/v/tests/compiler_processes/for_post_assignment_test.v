import os
import v.cmdexec

fn test_for_post_exits_drop_body_locals_once() {
	root := os.join_path(os.vtmp_dir(), 'for_post_exits_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	program := '
struct Tracked { id int }

fn (item &Tracked) free() {
	println("drop \${item.id}")
}

fn next_index(i int) !int {
	if i == 1 { return error("done") }
	return i + 1
}

fn check_break() {
	outer := Tracked{90}
	for i := 0; i < 3; i = next_index(i) or { break } {
		body := Tracked{10 + i}
		text := "body_" + i.str()
		defer { println(text) }
		assert body.id == 10 + i
		// BODY_CONTINUE
	}
	assert outer.id == 90
	println("after break")
}

fn check_return() {
	outer := Tracked{91}
	for i := 0; i < 3; i = next_index(i) or { return } {
		body := Tracked{20 + i}
		text := "body_" + i.str()
		defer { println(text) }
		assert body.id == 20 + i && outer.id == 91
		// BODY_CONTINUE
	}
	assert false
}

fn check_propagation() ! {
	outer := Tracked{92}
	for i := 0; i < 3; i = next_index(i)! {
		body := Tracked{30 + i}
		text := "body_" + i.str()
		defer { println(text) }
		assert body.id == 30 + i && outer.id == 92
		// BODY_CONTINUE
	}
	assert false
}

fn main() {
	check_break()
	check_return()
	check_propagation() or { println("propagated") }
}
'
	for translated in [false, true] {
		attribute := if translated { '@[translated]\n' } else { '' }
		source := os.join_path(root, 'main.v')
		for take_continue in [false, true] {
			code := program.replace('// BODY_CONTINUE', if take_continue { 'continue' } else { '' })
			os.write_file(source, '${attribute}module main\n${code}')!
			for autofree in [false, true] {
				for serial in [false, true] {
					mut args := ['-gc', 'none', '-cc', @CCOMPILER]
					if autofree { args << '-autofree' }
					if serial { args << '-no-parallel' }
					args << ['run', source]
					result := cmdexec.run(@VEXE, args)
					mode := 'translated=${translated}, autofree=${autofree}, serial=${serial}, continue=${take_continue}'
					assert result.exit_code == 0, '${mode}: ${result.output}'
					expected := if autofree {
						'body_0\ndrop 10\nbody_1\ndrop 11\nafter break\ndrop 90\n' +
							'body_0\ndrop 20\nbody_1\ndrop 21\ndrop 91\n' +
							'body_0\ndrop 30\nbody_1\ndrop 31\ndrop 92\npropagated\n'
					} else {
						'body_0\nbody_1\nafter break\nbody_0\nbody_1\nbody_0\nbody_1\npropagated\n'
					}
					assert result.output == expected, '${mode}: ${result.output}'
				}
			}
		}
	}
}

fn test_for_post_assignments_preserve_control_flow_and_cleanup() {
	root := os.join_path(os.vtmp_dir(), 'for_post_assignments_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	program := "
struct Cursor {
mut:
	index int
	calls int
	text string
}

fn advance(mut state Cursor) int {
	state.calls++
	return state.index + 1
}

fn slot(mut state Cursor) int {
	state.calls++
	return 0
}

fn recorded_post(mut state Cursor, mut seen []int) int {
	seen << 20 + state.index
	return state.index + 1
}

fn check_field_continue_and_break() {
	mut state := Cursor{index: 1}
	mut visits := 0
	for ; ; state.index = (state.index + 1) & 3 {
		visits++
		assert visits <= 4
		if state.index != 0 {
			continue
		}
		break
	}
	assert visits == 4
	assert state.index == 0
}

fn check_normal_fallthrough() {
	mut state := Cursor{}
	mut visits := 0
	for ; state.index < 4; state.index = advance(mut state) {
		visits++
		assert visits <= 4
	}
	assert visits == 4
	assert state.index == 4
	assert state.calls == 4
}

fn check_indexed_posts() {
	mut state := Cursor{}
	mut indices := [0]
	mut visits := 0
	for ; indices[0] < 4; indices[slot(mut state)] += 1 {
		visits++
		assert visits <= 4
		if indices[0] % 2 == 0 {
			continue
		}
	}
	assert indices == [4]
	assert visits == 4
	assert state.calls == 4
	mut fixed := [0]!
	visits = 0
	for ; fixed[0] < 2; fixed[0] = fixed[0] + 1 {
		visits++
		assert visits <= 2
		continue
	}
	assert fixed[0] == 2
}

fn check_string_compound_post() {
	mut state := Cursor{}
	mut visits := 0
	for ; state.text.len < 3; state.text += 'x' {
		visits++
		assert visits <= 3
		continue
	}
	assert visits == 3
	assert state.text == 'xxx'
}

fn check_nested_and_labelled_control() {
	mut outer := Cursor{}
	mut outer_visits := 0
	mut seen := []int{}
	outer_loop: for ; outer.index < 3; outer.index += 1 {
		outer_visits++
		assert outer_visits <= 3
		mut inner := Cursor{}
		mut inner_visits := 0
		for ; inner.index < 3; inner.index += 1 {
			inner_visits++
			assert inner_visits <= 3
			if inner.index == 0 {
				continue
			}
			seen << outer.index * 10 + inner.index
			if outer.index == 1 {
				continue outer_loop
			}
			break
		}
		assert inner.index == 1
		if outer.index == 0 {
			continue
		}
	}
	assert outer.index == 3
	assert outer_visits == 3
	assert seen == [1, 11, 21]
}

fn check_defer_before_post() {
	mut state := Cursor{}
	mut seen := []int{}
	mut visits := 0
	for ; state.index < 2; state.index = recorded_post(mut state, mut seen) {
		visits++
		assert visits <= 2
		label := 'body_' + state.index.str()
		defer {
			assert label.len == 6
			seen << 10 + state.index
		}
		seen << state.index
		if state.index == 0 {
			continue
		}
	}
	assert seen == [0, 10, 20, 1, 11, 21]
}

fn main() {
	check_field_continue_and_break()
	check_normal_fallthrough()
	check_indexed_posts()
	check_string_compound_post()
	check_nested_and_labelled_control()
	check_defer_before_post()
	println('for post assignments ok')
}
"
	for translated in [false, true] {
		attribute := if translated { '@[translated]\n' } else { '' }
		source := os.join_path(root, 'main.v')
		os.write_file(source, '${attribute}module main\n${program}')!
		for autofree in [false, true] {
			mut args := [@VEXE, '-gc', 'none']
			if autofree {
				args << '-autofree'
			}
			args << ['run', source]
			result := os.exec(args)
			assert result.exit_code == 0, 'translated=${translated}, autofree=${autofree}: ${result.output}'
			assert result.output == 'for post assignments ok\n', result.output
		}
	}
}
