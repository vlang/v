module fastc

import os
import v.cmdexec
import v.pref

fn test_removed_integer_callback_parameters_keep_the_replacement_diagnostic() {
	for selfhost in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.building_v = selfhost
		for spelling in ['isize', 'usize'] {
			replacement := if spelling == 'isize' { 'int' } else { 'uint' }
			for parameter in [
				'Legacy',
				'[]Legacy',
				'[2]Legacy',
				'?[]Legacy',
				'&[]Legacy',
				'map[string][]Legacy',
				'chan []Legacy',
				'thread []Legacy',
				'fn([]Legacy) int',
				'fn(value [2]Legacy) int',
				'Holder[Legacy]',
				'Holder[fn([]Legacy) int]',
			] {
				for declaration in ['type Callback = fn(Parameter) int',
					'struct Callbacks { callback fn(Parameter) int }'] {
					source := 'struct Holder[T] { mark int }\n' + declaration.replace('Parameter',
						parameter.replace('Legacy', spelling)) + '\nfn main() {}'
					_ := generate(source, 'removed_integer_callback.v', prefs) or {
						assert err.msg().contains('type `${spelling}`; use `${replacement}` instead'), err.msg()
						continue
					}
					assert false, source
				}
			}
		}
	}
}

fn test_integer_callback_parameter_names_remain_valid() {
	for selfhost in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.building_v = selfhost
		for signature in [
			'fn(usize uint, isize int) int',
			'fn(usize []uint, isize [2]int) int',
			'fn(usize ?[]uint, isize &[]int) int',
			'fn(usize fn([]uint) int, isize map[string][]int) int',
			'fn(usize Holder[uint], isize []fn([2]int) int) int',
			'fn(usize uint, []int, isize [2]uint) int',
			'fn(map []int) int',
			'fn(map [3]int) int',
			'fn(map [count]int) int',
			'fn(map [usize]int) int',
			'fn(map [isize]int) int',
			'fn(map [usize + 1]int) int',
			'fn(thread ?int) int',
			'fn(thread !int) int',
		] {
			source := 'const count = 3\nconst usize = 3\nconst isize = 3\nstruct Holder[T] { mark int }\ntype Callback = ${signature}\nstruct Callbacks { callback ${signature} }\nfn main() {}'
			_ := generate(source, 'named_integer_callback.v', prefs) or { panic(err) }
		}
	}
}

fn test_composite_removed_integer_callback_patterns_never_match() {
	root := os.join_path(os.vtmp_dir(), 'fastc_integer_callback_patterns_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Holder[T] { mark int }
const count = 3
const usize = 3
const isize = 3
type Dynamic = fn(map []int) int
type Fixed = fn(map [3]int) int
type Symbolic = fn(map [count]int) int
type UnsignedName = fn(map [usize]int) int
type SignedName = fn(map [isize]int) int
type NamedOption = fn(thread ?int) int
fn dynamic(map []int) int { return map.len }
fn optional(thread ?int) int { return 3 }
fn use_dynamic(callback Dynamic) int {
 values := [int(1), 2, 3]
 return callback(values)
}
fn use_optional(callback NamedOption) int {
 value := ?int(3)
 return callback(value)
}
fn canonical[T](value T) bool {
 $if T is fn([]usize) int || T is fn([2]isize) int || T is fn(?[]usize) int || T is fn(&[]isize) int || T is fn(map[string][]usize) int || T is fn(chan []isize) int || T is fn(thread []usize) int || T is fn(fn([2]isize) int) int || T is fn(Holder[usize]) int || T is fn(Holder[fn([]isize) int]) int || T is []fn([]usize) int || T is ?fn([]isize) int {
  return false
 } $else {
  $if T !is fn([]usize) int { return true } $else { return false }
 }
}
fn canonical_match[T](value T) bool {
 $match T {
  fn([]usize) int, fn([2]isize) int { return false }
  fn(fn([]usize) int) int { return false }
  $else { return true }
 }
}
fn main() {
 assert use_dynamic(dynamic) == 3
 assert use_optional(optional) == 3
 assert canonical(int(1))
 assert canonical(uint(1))
 assert canonical_match(int(1))
 assert canonical_match(uint(1))
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(program, [])
	assert ran.exit_code == 0, ran.output
}
