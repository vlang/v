module fastc

import os
import v.cmdexec

// `&T{...src}` copies `src` whether it is a value or a reference.
fn test_fastc_heap_struct_updates_from_values() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_heap_struct_updates_from_values_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

struct Config {
	name string
	size int
}

type Alias = Config

fn pick(c ?Config) &Alias {
	mut config_ := &Alias(unsafe { nil })
	if v := c {
		config_ = &Alias{
			...v
			size: v.size + 1
		}
	}
	return config_
}

fn main() {
	p := pick(Config{'a', 2})
	println(p.size)
	println(p.name)
	base := &Config{'b', 5}
	q := &Config{
		...base
		size: 6
	}
	println(q.size)
}

struct Iter {
	pos int
	max int
}

fn advance(it Iter) &Iter {
	return &Iter{
		...it
		pos: it.pos + 1
	}
}

fn init() {
	mut it := Iter{0, 3}
	n := advance(it)
	println(n.pos)
}
") or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '1\n3\na\n6', run.output
}
