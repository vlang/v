import os
import v.tempname

fn test_local_array_const_materialization() {
	root := os.join_path(os.vtmp_dir(), 'v3_fixed_array_const_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	bin := os.join_path(root, 'v3_fixed_array_const')
	src := bin + '.v'
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

const clone_vals = [u32(2), 3, 4]

fn bump(mut xs [3]u32) {
	xs[0] += clone_vals[1]
}

fn clone_score() int {
	mut xs := [u32(10), 20, 30]!
	bump(mut xs)
	cloned := clone_vals.clone()
	cloned_paren := (clone_vals).clone()
	return int(xs[0] + cloned[2] + cloned_paren[1])
}

const vals = [u32(1), 2, 3]
const fixed_only = [u32(4), 5, 6]

fn take(xs []u32) u32 {
	return xs[2]
}

fn indexed_score() u32 {
	xs := vals
	return vals[1] + xs[0] + take(vals) + fixed_only[1]
}

fn main() {
	println(sum_nums().str())
	println(item_sum().str())
	println(clone_score().str())
	println(indexed_score().str())
}
') or {
		panic(err)
	}
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['21', '15', '20', '11'], run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := generated.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('memmove(main__nums,'), generated
	assert compact.contains('memmove(main__items,'), generated
	// V `int` is `i64` on 64-bit targets.
	assert compact.contains('i64row[3];'), generated
	assert compact.contains('memmove(row,main__nums['), generated
	assert !compact.contains('i64row=main__nums['), generated
	// The nested loops must not get an empty `< 0` bound. Only `sum_nums` is
	// checked: builtin code such as `string__lt` returns `cmp < 0;`.
	assert generated.contains('i64 sum_nums(void) {'), generated
	sum_nums_body := generated.all_after('i64 sum_nums(void) {').all_before('\n}\n')
	assert !sum_nums_body.replace('\t', '').replace(' ', '').replace('\n', '').contains('<0;'), sum_nums_body
	deparenthesized := compact.replace('(', '').replace(')', '')
	assert deparenthesized.contains('returnmain__items[0]->value+main__items[1]->value;'), generated

	// Cloning requires a dynamic array even when another use indexes the constant.
	assert compact.contains('bump(&xs);'), generated
	assert !compact.contains('bump(xs);'), generated
	assert !compact.contains('u32main__clone_vals[3]'), generated
	assert !compact.contains('array__clone(&main__clone_vals)') || generated.contains('Array main__clone_vals'), generated
	assert !deparenthesized.contains('array__clone&main__clone_vals')
		|| generated.contains('Array main__clone_vals'), generated

	// Assignment and argument passing also require dynamic storage.
	assert generated.contains('Array main__vals'), generated
	assert !compact.contains('constu32main__vals[3]'), generated
	assert !compact.contains('Arrayxs=main__vals;') || generated.contains('Array main__vals'), generated
	// Cache-split builds keep inferred array constants dynamic to preserve the module ABI.
	assert compact.contains('constu32main__fixed_only[3]')
		|| generated.contains('Array main__fixed_only'), generated
}

fn test_imported_array_const_storage_uses_module_context() {
	root := os.join_path(os.vtmp_dir(), 'v3_imported_array_const_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	for sub in ['a', 'b', 'pkg'] {
		os.mkdir(os.join_path(root, sub))!
	}
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
	os.write_file(os.join_path(root, 'pkg', 'pkg.v'), 'module pkg

pub const values = [u32(1), 2, 3]

pub fn indexed() u32 {
	return values[1]
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

import a
import b
import pkg as p

fn take(x []int) int {
	return x[0]
}

fn take_dynamic(xs []u32) u32 {
	return xs[2]
}

fn main() {
	vals := [10, 20, 30]
	println((take(vals) + a.pick() + b.pick()).str())
	println((p.indexed() + take_dynamic(p.values)).str())
}
') or {
		panic(err)
	}
	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '17\n5', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := generated.replace('\t', '').replace(' ', '').replace('\n', '')
	// Cache-split builds keep inferred array constants dynamic to preserve the module ABI.
	assert compact.contains('consti64a__vals[3]') || generated.contains('Array a__vals'), generated
	assert compact.contains('consti64b__vals[3]') || generated.contains('Array b__vals'), generated
	assert generated.contains('Array pkg__values'), generated
	assert !compact.contains('constu32pkg__values[3]'), generated
}
