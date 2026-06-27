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
