import os

const mut_param_return_reassign_vexe = @VEXE
const mut_param_return_reassign_tests_dir = os.dir(@FILE)
const mut_param_return_reassign_v3_dir = os.dir(mut_param_return_reassign_tests_dir)
const mut_param_return_reassign_vlib_dir = os.dir(mut_param_return_reassign_v3_dir)
const mut_param_return_reassign_v3_src = os.join_path(mut_param_return_reassign_v3_dir, 'v3.v')

fn test_reassign_from_mut_parameter_return_does_not_drop_aliased_storage() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_mut_param_return_reassign_${pid}')
	source := os.join_path(os.temp_dir(), 'v3_mut_param_return_reassign_input_${pid}.v')
	output := os.join_path(os.temp_dir(), 'v3_mut_param_return_reassign_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(source) or {}
		os.rm(output) or {}
		os.rm(output + '.c') or {}
	}
	build :=
		os.execute('${mut_param_return_reassign_vexe} -gc none -d ownership -path "${mut_param_return_reassign_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${mut_param_return_reassign_v3_src}')
	assert build.exit_code == 0, build.output
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
		os.execute('${v3_bin} -ownership -d ownership -nocache -no-parallel ${source} -b c -keepc -o ${output}')
	assert compile.exit_code == 0, compile.output
	c_source := os.read_file(output + '.c') or { panic(err) }
	assert !c_source.contains('main__Owned __drop_assign'), c_source
	run := os.execute(output)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
