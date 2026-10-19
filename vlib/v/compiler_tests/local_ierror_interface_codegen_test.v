import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v.v')

fn v3_bin_path() string {
	return os.join_path(os.temp_dir(), 'v3_local_ierror_interface_test_${os.getpid()}') + $if windows { '.exe' } $else { '' }
}

fn testsuite_begin() {
	os.rm(v3_bin_path()) or {}
}

fn testsuite_end() {
	os.rm(v3_bin_path()) or {}
}

fn build_v3() string {
	v3_bin := v3_bin_path()
	if os.exists(v3_bin) {
		return v3_bin
	}
	build :=
		os.exec([vexe, '-gc', 'none', '-path', '${vlib_dir}' + '|@vlib|@vmodules', '-o', v3_bin,
			'${v3_src}'])
	assert build.exit_code == 0, build.output
	return v3_bin
}

// `IError` is reserved for the builtin interface. A module that declares its own
// `IError` is rejected, so its values can never be confused with builtin errors.
fn test_module_local_ierror_interface_is_rejected() {
	v3_bin := build_v3()

	root := os.join_path(os.temp_dir(), 'v3_local_ierror_interface_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	mod_dir := os.join_path(root, 'pkg')
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'pkg.v'), "module pkg

pub interface IError {
	name() string
}

pub struct LocalErr {
	text string
}

pub fn (err LocalErr) name() string {
	return err.text
}

pub fn make() IError {
	return LocalErr{
		text: 'local'
	}
}

pub fn show(err IError) string {
	return err.name()
}
") or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, 'import pkg

fn main() {
	println(pkg.show(pkg.make()))
}
') or {
		panic(err)
	}

	bin := os.join_path(root, 'program')
	compile := os.exec([v3_bin, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('pkg.v:3:15: error: cannot register interface `IError`, it is builtin interface type'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_builtin_ierror_payload_uses_builtin_interface() {
	v3_bin := build_v3()

	root := os.join_path(os.temp_dir(), 'v3_builtin_ierror_payload_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	src := os.join_path(root, 'main.v')
	os.write_file(src, 'struct BuiltinErr {
	text string
}

fn (err BuiltinErr) msg() string {
	return err.text
}

fn (err BuiltinErr) code() int {
	return 7
}

fn payload() !IError {
	return BuiltinErr{
		text: "builtin"
	}
}

fn main() {
	builtin := payload() or { panic(err) }
	println(builtin.msg())
	println(builtin.code())
}
') or {
		panic(err)
	}

	bin := os.join_path(root, 'program')
	compile := os.exec([v3_bin, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file('${bin}.c') or { '' }
	assert c_code.contains('typedef struct __v_result_IError {'), c_code

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'builtin\n7'
}
