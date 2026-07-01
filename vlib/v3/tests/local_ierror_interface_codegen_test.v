import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_local_ierror_interface_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_module_local_ierror_interface_is_not_builtin_ierror() {
	v3_bin := build_v3()

	root := os.join_path(os.temp_dir(), 'v3_local_ierror_interface_${os.getpid()}')
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

pub fn make_ref() &IError {
	return &IError(LocalErr{
		text: 'local-ref'
	})
}

pub fn show(err IError) string {
	return err.name()
}

pub fn show_ref(err &IError) string {
	return err.name()
}
	") or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, 'import pkg

fn main() {
	err := pkg.make()
	println(pkg.show(err))
	ref := pkg.make_ref()
	println(pkg.show_ref(ref))
}
	') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_local_ierror_interface_input')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file('${bin}.c') or { '' }
	assert c_code.contains('pkg__IError pkg__make'), c_code
	assert c_code.contains('string pkg__show(pkg__IError err)'), c_code
	assert c_code.contains('pkg__IError* pkg__make_ref'), c_code
	assert c_code.contains('string pkg__show_ref(pkg__IError* err)'), c_code
	assert !c_code.contains('\nIError pkg__make'), c_code
	assert !c_code.contains('string pkg__show(IError err)'), c_code
	assert !c_code.contains('\nIError* pkg__make_ref'), c_code
	assert !c_code.contains('string pkg__show_ref(IError* err)'), c_code

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'local\nlocal-ref'
}
