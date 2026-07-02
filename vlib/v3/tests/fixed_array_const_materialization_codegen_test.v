import os

const fixed_array_const_vexe = @VEXE
const fixed_array_const_tests_dir = os.dir(@FILE)
const fixed_array_const_v3_dir = os.dir(fixed_array_const_tests_dir)
const fixed_array_const_vlib_dir = os.dir(fixed_array_const_v3_dir)
const fixed_array_const_v3_src = os.join_path(fixed_array_const_v3_dir, 'v3.v')

fn fixed_array_const_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_fixed_array_const_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${fixed_array_const_vexe} -gc none -path "${fixed_array_const_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${fixed_array_const_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_fixed_array_const_materialization_and_nested_for_in() {
	v3_bin := fixed_array_const_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_fixed_array_const_${os.getpid()}.v')
	os.write_file(src, 'module main

struct Item {
	value int
}

const nums = [[1, 2, 3]!, [4, 5, 6]!]!
const items = [&Item{value: 7}, &Item{value: 8}]!

fn sum_nums() int {
	mut total := 0
	for row in nums {
		for n in row {
			total += n
		}
	}
	return total
}

fn item_sum() int {
	return items[0].value + items[1].value
}

fn main() {
	println(sum_nums().str())
	println(item_sum().str())
}
') or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_fixed_array_const_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['21', '15'], run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := generated.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('memmove(main__nums,'), generated
	assert compact.contains('memmove(main__items,'), generated
	assert compact.contains('introw[3];'), generated
	assert compact.contains('memmove(row,main__nums['), generated
	assert !compact.contains('introw=main__nums['), generated
	assert !compact.contains('<0;'), generated
	deparenthesized := compact.replace('(', '').replace(')', '')
	assert deparenthesized.contains('returnmain__items[0]->value+main__items[1]->value;'), generated
}

fn test_fixed_array_pointer_arg_and_dynamic_const_clone() {
	v3_bin := fixed_array_const_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_fixed_array_arg_const_clone_${os.getpid()}.v')
	os.write_file(src, 'module main

const vals = [u32(1), 2, 3]

fn bump(mut xs [3]u32) {
	xs[0] += vals[1]
}

fn score() int {
	mut xs := [u32(10), 20, 30]!
	bump(mut xs)
	cloned := vals.clone()
	cloned_paren := (vals).clone()
	return int(xs[0] + cloned[2] + cloned_paren[1])
}

fn main() {
	println(score().str())
}
') or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_fixed_array_arg_const_clone_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '17', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := generated.replace('\t', '').replace(' ', '').replace('\n', '')
	deparenthesized := compact.replace('(', '').replace(')', '')
	assert compact.contains('bump(&xs);'), generated
	assert !compact.contains('bump(xs);'), generated
	assert !compact.contains('u32main__vals[3]'), generated
	assert !compact.contains('array__clone(&main__vals)') || generated.contains('Array main__vals'), generated
	assert !deparenthesized.contains('array__clone&main__vals')
		|| generated.contains('Array main__vals'), generated
}

fn test_indexed_const_array_with_dynamic_use_stays_dynamic() {
	v3_bin := fixed_array_const_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_fixed_array_const_dynamic_use_${os.getpid()}.v')
	os.write_file(src, 'module main

const vals = [u32(1), 2, 3]
const fixed_only = [u32(4), 5, 6]

fn take(xs []u32) u32 {
	return xs[2]
}

fn score() u32 {
	xs := vals
	return vals[1] + xs[0] + take(vals) + fixed_only[1]
}

fn main() {
	println(score().str())
}
') or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_fixed_array_const_dynamic_use_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '11', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := generated.replace('\t', '').replace(' ', '').replace('\n', '')
	assert generated.contains('Array main__vals'), generated
	assert !compact.contains('constu32main__vals[3]'), generated
	assert !compact.contains('Arrayxs=main__vals;') || generated.contains('Array main__vals'), generated
	assert compact.contains('constu32main__fixed_only[3]'), generated
}

fn test_ambiguous_short_const_arrays_resolve_in_module_context() {
	v3_bin := fixed_array_const_build_v3()
	root := os.join_path(os.temp_dir(), 'v3_fixed_array_ambiguous_const_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'a')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'b')) or { panic(err) }
	os.write_file(os.join_path(root, 'a', 'a.v'), 'module a

const vals = [1, 2, 3]

pub fn pick() int {
	return vals[1]
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'b', 'b.v'), 'module b

const vals = [4, 5, 6]

pub fn pick() int {
	return vals[1]
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

import a
import b

fn take(x []int) int {
	return x[0]
}

fn main() {
	vals := [10, 20, 30]
	println((take(vals) + a.pick() + b.pick()).str())
}
') or {
		panic(err)
	}
	bin := os.join_path(root, 'app')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '17', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := generated.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('constinta__vals[3]'), generated
	assert compact.contains('constintb__vals[3]'), generated
}
