module fastc

import os
import v.cmdexec

// `$for` fields answer the kind tests (`field.is_map`, `.is_struct`, ...) on their type
// without option, pointer and alias wrappers, as the C backend does.
fn test_fastc_comptime_field_kind_flags() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_comptime_field_flags_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

enum Mode {
	a
	b
}

type Names = []string

struct Inner {
	x int
}

struct Config {
mut:
	count   int
	tags    map[string]int
	names   Names
	ptr     &Inner = unsafe { nil }
	inner   Inner
	maybe   ?Inner
	mode    Mode
	values  [2]int
pub:
	visible bool
}

fn describe[T](c T) []string {
	mut out := []string{}
	$for field in T.fields {
		mut kinds := []string{}
		$if field.is_map {
			kinds << 'map'
		}
		$if field.is_array {
			kinds << 'array'
		}
		$if field.is_option {
			kinds << 'option'
		}
		$if field.is_struct {
			kinds << 'struct'
		}
		$if field.is_enum {
			kinds << 'enum'
		}
		$if field.is_pub {
			kinds << 'pub'
		}
		$if field.is_mut {
			kinds << 'mut'
		}
		out << field.name + ':' + kinds.join(',')
	}
	return out
}

fn reset[T](mut c T, defaults T) {
	$for field in T.fields {
		$if !field.is_map {
			$if field.name == 'count' {
				c.$(field.name) = defaults.$(field.name)
			}
		}
	}
}

fn main() {
	for line in describe(Config{}) {
		println(line)
	}
	mut c := Config{count: 3}
	reset(mut c, Config{count: 9})
	println(c.count)
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
	assert run.output.trim_space() == 'count:mut\ntags:map,mut\nnames:array,mut\nptr:struct,mut\ninner:struct,mut\nmaybe:option,struct,mut\nmode:enum,mut\nvalues:array,mut\nvisible:pub\n9', run.output
}
