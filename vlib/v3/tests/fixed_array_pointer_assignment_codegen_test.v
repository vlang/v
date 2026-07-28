import os

const fixed_array_pointer_assignment_vexe = @VEXE
const fixed_array_pointer_assignment_tests_dir = os.dir(@FILE)
const fixed_array_pointer_assignment_v3_dir = os.dir(fixed_array_pointer_assignment_tests_dir)
const fixed_array_pointer_assignment_vlib_dir = os.dir(fixed_array_pointer_assignment_v3_dir)
const fixed_array_pointer_assignment_v3_src = os.join_path(fixed_array_pointer_assignment_v3_dir,
	'v3.v')

fn test_fixed_array_pointer_assignment_targets_element() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_fixed_array_pointer_assignment_${os.getpid()}')
	build :=
		os.execute('${fixed_array_pointer_assignment_vexe} -gc none -prealloc -path "${fixed_array_pointer_assignment_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${fixed_array_pointer_assignment_v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_fixed_array_pointer_assignment_${os.getpid()}.v')
	os.write_file(src, 'fn pair() (int, int) {
	return 5, 6
}

fn update(values &[3]int) {
	unsafe {
		values[1] = 40
		values[1] += 2
		values[0], values[2] = pair()
	}
}

fn main() {
	mut values := [1, 2, 3]!
	unsafe {
		update(&values)
		println(values[0] + values[1] + values[2])
	}
}
') or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_fixed_array_pointer_assignment_program_${os.getpid()}')
	compile := os.execute('${v3_bin} -nocache -no-parallel -keepc ${src} -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '53', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := generated.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('(*values)[1]=40;'), generated
	assert compact.contains('(*values)[1]+=2;'), generated
	assert compact.contains('(*values)[0]='), generated
	assert compact.contains('(*values)[2]='), generated
}
