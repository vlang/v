module fastc

import os
import v.cmdexec

// As in V, a generic instance for a pointer is neither `$struct` nor `$enum`, so
// `$for` loops over struct members are not unrolled for it.
fn test_fastc_pointer_instances_are_not_structs_or_enums() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_pointer_type_groups_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

struct Set {
	items []int
}

fn (s &Set) equal(other &Set) bool {
	return s.items == other.items
}

struct Pair {
	a int
	b string
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
	return false
}

fn main() {
	a := &Set{[1, 2]}
	b := &Set{[1, 2]}
	c := &Set{[3]}
	println(deep_equal(a, b))
	println(deep_equal(a, c))
	p := &Pair{1, 'x'}
	q := &Pair{1, 'y'}
	println(deep_equal(p, p))
	println(deep_equal(p, q))
}

enum Kind {
	a
	b
}

fn kind_of[T](v T) string {
	$if T is $enum {
		return 'enum'
	} $else $if T is $struct {
		return 'struct'
	} $else $if T is $pointer {
		return 'pointer'
	}
	return 'other'
}

fn init() {
	k := Kind.b
	println(kind_of(k))
	println(kind_of(&k))
	println(kind_of(Pair{}))
	println(kind_of(&Pair{}))
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
	assert run.output.trim_space() == 'enum\npointer\nstruct\npointer\nfalse\nfalse\nfalse\nfalse', run.output
}
