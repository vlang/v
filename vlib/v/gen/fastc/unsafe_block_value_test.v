module fastc

import os
import v.cmdexec
import v.pref

// An `unsafe { ... }` block used as a value may declare locals before its value, as
// the stack-buffer key in vlib/v/types/checker_tail.v does inside an if-expression.
// It is lowered as a statement expression rather than read transparently.
fn test_fastc_unsafe_block_with_statements_as_a_value() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_unsafe_block_value_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

fn joined_key(prefix string, name string) string {
	mut key_buf := [32]u8{}
	key_len := prefix.len + 1 + name.len
	key := if key_len <= key_buf.len {
		unsafe {
			key_start := &u8(&key_buf[0])
			vmemcpy(key_start, prefix.str, prefix.len)
			key_start[prefix.len] = `.`
			vmemcpy(key_start + prefix.len + 1, name.str, name.len)
			tos(key_start, key_len)
		}
	} else {
		prefix + "." + name
	}
	return key.clone()
}

fn block_values(n int) int {
	x := unsafe {
		mut a := n + 1
		a *= 2
		a
	}
	y := match n {
		1 {
			unsafe {
				b := n * 10
				b + 1
			}
		}
		else {
			7
		}
	}
	return x + y
}

fn main() {
	assert joined_key("mod", "name") == "mod.name"
	assert joined_key("m".repeat(40), "x").len == 42
	assert block_values(1) == 15
	assert block_values(2) == 13
	println("ok")
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	c_source := generate_files([root], prefs) or { panic(err) }
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
