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
struct IndexedValues { indexed []uint }
fn indexed[T](value T) T { return value }
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
 items := [uint(1), 2]
 assert items[usize] == 2
 assert items[isize + 1] == 1
 indexed_values := IndexedValues{indexed: items}
 assert indexed_values.indexed[usize] == 2
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

fn test_composite_removed_integer_patterns_never_match() {
	root := os.join_path(os.vtmp_dir(), 'fastc_integer_patterns_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main
struct Box[T] { value T }
fn canonical[T](value T) bool {
 $if T is []usize || T is []isize || T is [2]usize || T is [2]isize || T is ?usize || T is ?isize || T is &usize || T is &isize || T is map[string][]usize || T is map[isize]uint || T is Box[usize] {
  return false
 } $else {
  $if T !is []usize { return true } $else { return false }
 }
}
fn canonical_match[T](value T) bool {
 $match T {
  []usize, []isize { return false }
  ?usize, ?isize { return false }
  &usize, &isize { return false }
  map[string][]usize { return false }
  $else { return true }
 }
}
fn main() {
 assert canonical(int(1))
 assert canonical(uint(1))
 assert canonical([uint(1)])
 assert canonical_match(int(1))
 assert canonical_match([uint(1)])
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(program, [])
	assert ran.exit_code == 0, ran.output
}

fn test_inactive_explicit_generics_do_not_reject_removed_integers() {
	root := os.join_path(os.vtmp_dir(), 'fastc_inactive_integer_generics_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main
struct Box[T] { value T }
fn identity[T](value T) T { return value }
$if false {
 fn inactive() { _ = identity[usize](1)\n _ = Box[isize]{value: 1} }
}
fn main() {
 $if @BACKEND == "c" {
  _ = identity[usize](1)
  _ = Box[isize]{value: 1}
 }
 $if false {
  _ = identity[usize](1)
  _ = Box[isize]{value: 1}
 } $else $if true {
  $if !fastc_backend ? {
   _ = identity[isize](1)
   _ = Box[usize]{value: 1}
  }
  assert identity[uint](uint(7)) == 7
  assert Box[uint]{value: 9}.value == 9
 } $else {
  _ = identity[isize](1)
  _ = Box[usize]{value: 1}
 }
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(program, [])
	assert ran.exit_code == 0, ran.output
}

fn test_integer_names_in_match_expressions_and_layout_queries() {
	root := os.join_path(os.vtmp_dir(), 'fastc_integer_match_layout_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main
struct Packet { prefix u8; usize u64; isize i64 }
struct CanonicalPacket { prefix u8; unsigned u64; signed i64 }
type Value = int | string
fn check_offset(actual int, expected int) bool { return actual == expected }
fn main() {
 unsigned_offset := int(__offsetof(CanonicalPacket, unsigned))
 signed_offset := int(__offsetof(CanonicalPacket, signed))
 assert __offsetof(Packet, usize) == unsigned_offset
 assert check_offset(int(__offsetof(Packet, isize)), signed_offset)
 usize := Value(7)
 x := match usize {
  int { usize + 1 }
  string { usize.len }
 }
 assert x == 8
 isize := Value("abc")
 y := match isize {
  int { isize + 1 }
  string { isize.len }
 }
 assert y == 3
 assert __offsetof(Packet, usize) == unsigned_offset
 assert __offsetof(Packet, isize) == signed_offset
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(program, [])
	assert ran.exit_code == 0, ran.output
}

fn test_removed_integer_names_in_generic_argument_value_contexts() {
	root := os.join_path(os.vtmp_dir(), 'fastc_integer_argument_contexts_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'helper')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'helper', 'helper.v'), 'module helper\npub struct Value { pub: n int }\n') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main
import helper as usize
type Combined = usize.Value | string
struct Holder[T] { mark int }
struct Values {
 qualified Holder[usize.Value]
 callback Holder[fn(usize int) int]
 named Holder[fn(isize int) int]
}
fn matches_import[T](value T) bool {
 $if T is usize.Value { return true } $else { return false }
}
fn rejects_legacy[T](value T) bool {
 $if T is usize { return false } $else { return true }
}
fn identity[T](value T) T { return value }
fn main() {
 values := Values{}
 assert values.qualified.mark == 0
 assert values.callback.mark == 0
 assert values.named.mark == 0
 assert identity[usize.Value](usize.Value{n: 3}).n == 3
 assert matches_import[usize.Value](usize.Value{n: 3})
 assert rejects_legacy[usize.Value](usize.Value{n: 3})
 combined := Combined(usize.Value{n: 3})
 assert combined is usize.Value
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(program, [])
	assert ran.exit_code == 0, ran.output
}
