module fastc

import os
import v.cmdexec

// `$array` matches fixed-size arrays as well as dynamic ones, so a generic `deep_equal` on a
// `[4]Rgb` takes its element loop instead of comparing with C `==`.
fn test_fastc_comptime_array_group_matches_fixed_arrays() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_comptime_array_group_matches_fixed_array_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

struct Rgb {
	r u8
	g u8
	b u8
}

fn deep_equal[T](old T, new T) bool {
	$if T is string {
		return old == new
	} $else $if T is $array {
		if old.len != new.len {
			return false
		}
		for i in 0 .. old.len {
			if !deep_equal(old[i], new[i]) {
				return false
			}
		}
		return true
	} $else $if T is $struct {
		$for field in T.fields {
			if !deep_equal(old.$(field.name), new.$(field.name)) {
				return false
			}
		}
		return true
	} $else {
		return old == new
	}
}

fn kind[T](v T) string {
	$if T is $array_dynamic {
		return 'dynamic'
	} $else $if T is $array_fixed {
		return 'fixed'
	}
	return 'other'
}

fn main() {
	mut a := [4]Rgb{}
	mut b := [4]Rgb{}
	println(deep_equal(a, b))
	b[2] = Rgb{1, 2, 3}
	println(deep_equal(a, b))
	println(deep_equal([1, 2], [1, 2]))
	println(kind([1, 2]))
	println(kind(a))
	println(kind(3))
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
	assert run.output.trim_space() == 'true\nfalse\ntrue\ndynamic\nfixed\nother', run.output
}
