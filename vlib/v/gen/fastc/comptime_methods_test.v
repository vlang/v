module fastc

import os
import v.cmdexec

// `$for method in T.methods` iterates the methods with a receiver, substituting
// `method.name`, `x.$method(...)` and `method.return_type is T`. The methods a loop
// selects by name stay reachable.
fn test_fastc_comptime_method_loops() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_comptime_methods_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

struct Entries {
	items []int
}

fn (e Entries) equal(other Entries) bool {
	return e.items == other.items
}

struct Holder {
	map Entries
}

fn (h Holder) equal(other Holder) bool {
	return h.map.equal(other.map)
}

struct Plain {
	a int
}

fn deep_equal[T](old T, new T) bool {
	$if T is $struct {
		$for method in T.methods {
			$if method.name == 'equal' {
				return old.$method(new)
			}
		}
		$for field in T.fields {
			if old.$(field.name) != new.$(field.name) {
				return false
			}
		}
		return true
	}
	return old == new
}

fn main() {
	println(deep_equal(Holder{Entries{[1]}}, Holder{Entries{[1]}}))
	println(deep_equal(Holder{Entries{[1]}}, Holder{Entries{[2]}}))
	println(deep_equal(Plain{1}, Plain{1}))
}

enum Color {
	red
	green
}

fn (c Color) format_entry() string {
	return 'color=\${c}'
}

struct Multi {
	n int
}

fn (m Multi) format_entry() []string {
	return ['a\${m.n}', 'b\${m.n}']
}

fn Multi.new() Multi {
	return Multi{1}
}

struct Maybe {}

fn (m Maybe) format_entry() ?string {
	return 'maybe'
}

fn entries[T](value T) []string {
	$for method in T.methods {
		$if method.name == 'format_entry' && method.return_type is string {
			return [value.format_entry()]
		} $else $if method.name == 'format_entry' && method.return_type is []string {
			return value.format_entry()
		} $else $if method.name == 'format_entry' && method.return_type is ?string {
			return [value.format_entry() or { '' }]
		}
	}
	return ['none']
}

fn count_methods[T]() int {
	mut n := 0
	$for method in T.methods {
		_ = method.name
		n++
	}
	return n
}

fn init() {
	println(entries(Color.green))
	println(entries(Multi{3}))
	println(entries(Maybe{}))
	println(entries(Plain{2}))
	println(count_methods[Multi]())
	println(count_methods[Plain]())
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
	assert run.output.trim_space() == "['color=green']\n['a3', 'b3']\n['maybe']\n['none']\n1\n0\ntrue\nfalse\ntrue", run.output
}
