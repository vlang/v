// vtest ownership
import os
import v.cmdexec
import v.tempname

fn test_reassign_from_mut_parameter_return_does_not_drop_aliased_storage() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_mut_param_return_reassign_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_mut_param_return_reassign_input.v')
	output := os.join_path(root, 'v3_mut_param_return_reassign_program')
	os.write_file(source, "struct Owned implements IClone {
mut:
	values []int
}

struct Extender {}

struct Parser {
	extender Extender
}

fn (extender Extender) extend(mut value Owned, mut other Owned) Owned {
	_ = extender
	_ = other
	value.values << 2
	return value
}

fn main() {
	mut value := Owned{
		values: [1]
	}
	mut other := Owned{}
	parser := Parser{}
	value = parser.extender.extend(mut value, mut other)
	assert value.values == [1, 2]
	println('ok')
}
") or {
		panic(err)
	}
	compile :=
		cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-d', 'ownership', '-nocache',
			'-no-parallel', source, '-b', 'c', '-keepc', '-o', output], '', {
			'VFLAGS':               ''
			'VTOOLS_CACHE_VERBOSE': ''
		})
	assert compile.exit_code == 0, compile.output
	c_source := os.read_file(output + '.c') or { panic(err) }
	assert !c_source.contains('main__Owned __drop_assign'), c_source
	run := os.exec([output])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
