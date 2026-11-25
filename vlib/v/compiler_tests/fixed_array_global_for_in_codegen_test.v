import os
import v.tempname

fn test_fixed_array_global_for_in_uses_fixed_array_indexing() {
	root := os.join_path(os.vtmp_dir(), 'v3_fixed_array_global_for_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	bin := os.join_path(root, 'v3_fixed_array_global_for')
	src := bin + '.v'
	os.write_file(src, 'module main

__global gnums = [6, 7, 8]!

fn sum_gnums() int {
	mut total := 0
	for n in gnums {
		total += n
	}
	return total
}

fn main() {
	println(sum_gnums().str())
}
') or {
		panic(err)
	}
	compile := os.exec([@VEXE, '-enable-globals', src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '21', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := generated.replace('\t', '').replace(' ', '').replace('\n', '')
	// A constant fixed-array global is initialized at file scope, and V `int` is
	// `i64` on 64-bit targets.
	assert compact.contains('i64gnums[3]={6,7,8};'), generated
	assert !compact.contains('memmove(gnums,'), generated
	assert compact.contains('=gnums['), generated
	assert !compact.contains('array_get(gnums,'), generated
}

fn test_imported_fixed_array_global_init_uses_imported_module_context() {
	root := os.join_path(os.vtmp_dir(), 'v3_fixed_array_imported_global_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.mkdir(os.join_path(root, 'm'))!
	os.write_file(os.join_path(root, 'main.v'), 'module main

import m

fn main() {
	println(int_str(m.sum()))
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'm', 'm.v'), 'module m

const seed = 5
__global table = [seed, 0]!

pub fn sum() int {
	return table[0]
}
') or {
		panic(err)
	}
	bin := os.join_path(root, 'app')
	compile :=
		os.exec([@VEXE, '-enable-globals', os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '5', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := generated.replace('\t', '').replace(' ', '').replace('\n', '')
	// `seed` resolves to the const of module `m` in the file-scope initializer.
	assert compact.contains('i64m__table[2]={5,0};'), generated
	assert !compact.contains('memmove(m__table,'), generated
}
