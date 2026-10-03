import os

const borrowed_array_field_vexe = @VEXE

fn test_ownership_borrowed_array_fields() {
	project := os.join_path(os.temp_dir(), 'v3_borrowed_array_field_${os.getpid()}')
	os.mkdir_all(project)!
	defer {
		os.rmdir_all(project) or {}
	}
	source := os.join_path(project, 'main.v')
	os.write_file(source, r'
struct Worker[T] {
	value T
}

struct Factory[T] {
	workers &[]Worker[T]
}

fn (factory &Factory[T]) get(index int) T {
	return factory.workers[index].value
}

fn main() {
	workers := [Worker[int]{value: 7}, Worker[int]{value: 11}]
	factory := Factory[int]{workers: &workers}
	assert factory.get(0) == 7
	assert factory.get(1) == 11
	assert workers[0].value == 7
	println("ok")
}
')!

	for mode in ['-no-parallel', ''] {
		out := os.execute('${borrowed_array_field_vexe} -new-compiler -nocache -cc clang -ownership ${mode} run ${source}')
		assert out.exit_code == 0, '${mode}: ${out.output}'
		assert out.output.ends_with('ok\n'), out.output
	}
}
