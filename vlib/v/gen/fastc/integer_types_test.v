module fastc

import os
import v.cmdexec
import v.pref

fn test_removed_integer_types_are_rejected_by_the_single_source_api() {
	for selfhost in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.building_v = selfhost
		for spelling in ['isize', 'usize'] {
			for template in [
				'fn main() { value := Legacy(1)\nprintln(value) }',
				'fn main() { Legacy(1) }',
				'fn value(x Legacy) {}\nfn main() {}',
				'struct Holder { value Legacy }\nfn main() {}',
				'fn main() { values := []Legacy{}\nprintln(values.len) }',
				'type Value = Legacy | string\nfn main() {}',
			] {
				source := template.replace('Legacy', spelling)
				_ := generate(source, 'removed_integer.v', prefs) or {
					assert err.msg().contains('does not support'), err.msg()
					assert err.msg().contains('`${spelling}`'), err.msg()
					continue
				}
				assert false, source
			}
		}
	}
}

fn test_removed_type_spellings_still_work_as_variable_and_field_names() {
	for selfhost in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.building_v = selfhost
		_ := generate('struct Names { usize uint; isize int }
fn main() {
	usize := uint(1)
	isize := int(-1)
	values := Names{usize: uint(1), isize: int(-1)}
	assert usize == 1
	assert isize == -1
	assert values.usize == 1
	assert values.isize == -1
}', 'integer_identifier_names.v', prefs) or { panic(err) }
	}
}

fn test_removed_type_spellings_still_work_as_constant_names() {
	for selfhost in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.building_v = selfhost
		_ := generate('const usize = uint(1)
const isize = int(-1)
fn main() {
 assert usize == 1
 assert isize == -1
}', 'integer_constant_names.v', prefs) or { panic(err) }
	}
}

fn test_integer_value_names_compile_and_run_with_canonical_types() {
	root := os.join_path(os.vtmp_dir(), 'fastc_integer_value_names_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main
struct Names { usize uint; isize int }
const usize = uint(1)
const isize = int(-1)
fn check_constants() {
 assert usize == 1
 assert isize == -1
}
fn add(usize uint, isize int) int { return int(usize) + isize }
fn maybe() ?uint { return uint(3) }
struct IntegerContainers { values []uint; maybe ?uint }
fn canonical_fields() bool {
 mut matches := 0
 $for field in Names.fields {
  $if field.typ is isize || field.typ is usize { return false }
  $if field.typ is uint { matches++ }
 }
 $for field in IntegerContainers.fields {
  $if field.typ is []usize || field.typ is ?usize { return false }
  $if field.typ is []uint { matches++ }
  $if field.typ is ?uint { matches++ }
 }
 return matches == 3
}
fn canonical[T](value T) bool {
 $if T is isize || T is usize { return false } $else { return true }
}
fn canonical_match[T](value T) bool {
 $match T {
  isize { return false }
  usize { return false }
  $else { return true }
 }
}

fn main() {
 check_constants()
 assert canonical_fields()
 assert canonical(int(-1))
 assert canonical(uint(1))
 assert canonical_match(int(-1))
 assert canonical_match(uint(1))
 assert add(uint(4), -1) == 3
 for usize in 0 .. 2 { assert uint(usize) < 2 }
 for isize, usize in [uint(1), 2] { assert usize == uint(isize + 1) }
 if usize := maybe() { assert usize == uint(3) } else { assert false }
 usize := uint(1)
 isize := int(-1)
 values := Names{usize: uint(1), isize: int(-1)}
 assert values.usize == usize
 assert values.isize == isize
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(program, [])
	assert ran.exit_code == 0, ran.output
}

fn test_removed_type_spellings_do_not_rename_native_c_fields() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_native_integer_names_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'native.h'), '#include <stdint.h>\ntypedef struct RawNames { uintptr_t usize; intptr_t isize; } RawNames;\nstatic RawNames raw_names(void) { return (RawNames){42, -1}; }\n') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main
#insert "native.h"
struct C.RawNames {
 usize C.uintptr_t
 isize C.intptr_t
}
fn C.raw_names() C.RawNames
fn main() {
 value := C.raw_names()
 assert uint(value.usize) == 42
 assert int(value.isize) == -1
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(program, [])
	assert ran.exit_code == 0, ran.output
}
