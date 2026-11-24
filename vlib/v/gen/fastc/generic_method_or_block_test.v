module fastc

import os
import v.cmdexec

// A generic method call is rendered as its specialization (`d.check_mono_string(...)`).
// Its `or` block must still see the result type, so `err` is bound and `return err`
// propagates the error, as json2's `decode_sumtype` does.
fn test_fastc_generic_method_or_block_binds_err() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_generic_method_or_block_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), r'module main

struct Decoder {
mut:
	depth int
}

fn (mut d Decoder) check[T](mut val T, n int) ! {
	if n > 2 {
		return error("too deep: ${n}")
	}
}

fn (mut d Decoder) read[T](mut val T, n int) !int {
	d.check(mut val, n)!
	return n * 2
}

fn (mut d Decoder) nested[T](mut val T, n int) ! {
	d.depth++
	d.check(mut val, n) or {
		d.depth--
		return err
	}
	d.check[T](mut val, n) or {
		d.depth--
		return error("explicit: ${err}")
	}
	d.depth--
}

fn main() {
	mut d := Decoder{}
	mut val := "x"
	d.nested(mut val, 1) or { panic(err) }
	d.nested(mut val, 5) or { println("${err.msg()} ${d.depth}") }
	d.check(mut val, 7) or { println(err.msg()) }
	d.check[string](mut val, 8) or { println(err.msg()) }
	println(d.read(mut val, 2) or { -1 })
	println(d.read(mut val, 3) or { -1 })
	value := d.read(mut val, 4) or {
		println(err.msg())
		0
	}
	println(value)
}
') or {
		panic(err)
	}
	program := os.join_path(root, 'program')
	// VFLAGS from a parent GCC/UBSan shard must not select an unsupported FastC C compiler.
	mut clean_env := map[string]string{}
	clean_env['VFLAGS'] = ''
	clean_env['VOSARGS'] = ''
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root],
		@VEXEROOT, clean_env)
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['too deep: 5 0', 'too deep: 7', 'too deep: 8',
		'4', '-1', 'too deep: 4', '0'], run.output
}
