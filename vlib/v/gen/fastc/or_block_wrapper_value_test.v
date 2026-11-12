module fastc

import os
import v.cmdexec

fn test_fastc_final_or_block_wrappers_preserve_values_and_nested_statements() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_or_block_wrapper_value_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

const wrapper_bytes = $if true {
	[
		u8(1), 2,
		3
	]!
} $else {
	[u8(0), 0, 0]!
}

fn missing() !int {
	return error("missing")
}

fn missing_void() ! {
	return error("missing void")
}

struct Counter {
mut:
	n int
}

fn touch(mut counter Counter) int {
	counter.n++
	return counter.n
}

fn void_call_handlers() {
	mut counter := Counter{}
	missing_void() or {
		assert err.msg() == "missing void"
		unsafe { touch(mut counter) }
	}
	assert counter.n == 1
	missing_void() or {
		$if true { touch(mut counter) }
	}
	assert counter.n == 2
	missing_void() or { touch(mut counter) }
	assert counter.n == 3
}

fn void_handlers() int {
	mut total := 0
	missing_void() or {
		$if false { total += 100 }
	}
	missing_void() or {
		$if true {
			assert err.msg() == "missing void"
			total += 1
		}
	}
	missing_void() or {
		unsafe {
			$if false { total += 100 }
		}
	}
	missing_void() or {
		$if true {
			unsafe {
				if total == 1 { total += 2 } else { total += 100 }
			}
		}
	}
	missing_void() or {
		if total == 3 { total += 4 } else { total += 100 }
	}
	missing_void() or {
		match total { 7 { total += 8 } else { total += 100 } }
	}
	return total
}

fn final_comptime(b bool) int {
	return missing() or {
		$if true {
			if b { 1 } else { 2 }
		} $else {
			3
		}
	}
}

fn final_comptime_else(b bool) int {
	return missing() or {
		$if false {
			3
		} $else {
			match b { true { 4 } else { 5 } }
		}
	}
}

fn final_comptime_chain(b bool) int {
	return missing() or {
		$if false {
			3
		} $else $if true {
			if b { 6 } else { 7 }
		} $else {
			8
		}
	}
}

fn final_unsafe(b bool) int {
	return missing() or {
		start := 8
		unsafe { if b { start } else { start + 1 } }
	}
}

fn comptime_loop(b bool) int {
	return missing() or {
		$if true {
			mut total := 0
			for i in 0 .. 3 {
				step := i + 1
				if i == 1 { total += step * 10 } else { total += step }
			}
			if b { total } else { total + 100 }
		} $else {
			0
		}
	}
}

fn unsafe_loop(b bool) int {
	return missing() or {
		start := 1
		unsafe {
			mut total := start
			for i in 0 .. 3 {
				step := i + 1
				match i { 1 { total += step * 10 } else { total += step } }
			}
			match b { true { total } else { total + 100 } }
		}
	}
}

fn nested_wrappers(b bool) int {
	return missing() or {
		start := 1
		unsafe {
			mut total := start
			$if true {
				for i in 0 .. 3 {
					step := i + 1
					if i < 2 { total += step } else { total += 10 }
				}
				if b { total } else { total + 100 }
			} $else {
				0
			}
		}
	}
}

fn nonfinal_wrappers() int {
	return missing() or {
		mut total := 0
		$if true {
			if total == 0 { total += 1 } else { total += 100 }
		}
		unsafe {
			match total { 1 { total += 2 } else { total += 100 } }
		}
		total
	}
}

struct Box { number int }

fn missing_box() !Box {
	return error("missing box")
}

fn comptime_return() !Box {
	box := missing_box() or {
		$if true {
			return err
		} $else {
			Box{number: 1}
		}
	}
	return box
}

fn unsafe_return() !Box {
	box := missing_box() or {
		assert err.msg() == "missing box"
		unsafe { return err }
	}
	return box
}

fn nested_return() !Box {
	box := missing_box() or {
		assert err.msg() == "missing box"
		unsafe {
			$if true {
				return err
			} $else {
				Box{number: 1}
			}
		}
	}
	return box
}

fn main() {
	assert wrapper_bytes == [u8(1), 2, 3]!
	assert void_handlers() == 15
	void_call_handlers()
	assert final_comptime(true) == 1
	assert final_comptime(false) == 2
	assert final_comptime_else(true) == 4
	assert final_comptime_else(false) == 5
	assert final_comptime_chain(true) == 6
	assert final_comptime_chain(false) == 7
	assert final_unsafe(true) == 8
	assert final_unsafe(false) == 9
	assert comptime_loop(true) == 24
	assert comptime_loop(false) == 124
	assert unsafe_loop(true) == 25
	assert unsafe_loop(false) == 125
	assert nested_wrappers(true) == 14
	assert nested_wrappers(false) == 114
	assert nonfinal_wrappers() == 3
	if _ := comptime_return() { panic("expected error") } else {
		assert err.msg() == "missing box"
	}
	if _ := unsafe_return() { panic("expected error") } else {
		assert err.msg() == "missing box"
	}
	if _ := nested_return() { panic("expected error") } else {
		assert err.msg() == "missing box"
	}
	println("or block wrapper values work")
}
')!
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, backend)
		build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-o', program, root])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'or block wrapper values work', run.output
	}
}
