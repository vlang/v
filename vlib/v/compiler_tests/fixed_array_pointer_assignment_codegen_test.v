import os
import v.tempname

fn test_fixed_array_pointer_assignment_targets_element() {
	root := os.join_path(os.vtmp_dir(), 'v3_fixed_array_pointer_assignment_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'main.v')
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
	bin := os.join_path(root, 'program')
	compile := os.exec([@VEXE, '-nocache', '-no-parallel', '-keepc', '-b', 'c', '${src}', '-o',
		bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '53', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := generated.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('(*values)[1]=40;'), generated
	assert compact.contains('(*values)[1]+=2;'), generated
	assert compact.contains('(*values)[0]='), generated
	assert compact.contains('(*values)[2]='), generated
}
