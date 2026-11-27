import os
import v.tempname

fn test_ierror_str_uses_builtin_formatter() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_str_codegen_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }

	src := os.join_path(root, 'v3_ierror_str_codegen_input.v')
	os.write_file(src, "fn fail_code(code int) !int {
	return error_with_code('hi', code)
}

fn fail_zero() !int {
	return error_with_code('hi', 0)
}

fn render(err IError) string {
	return err.str()
}

fn main() {
	fail_code(137) or {
		assert err.str() == 'hi; code: 137'
		assert err.msg() == 'hi'
		assert err.code() == 137
		assert render(err) == 'hi; code: 137'
	}
	fail_zero() or {
		assert err.str() == 'hi'
		assert err.msg() == 'hi'
		assert err.code() == 0
		assert render(err) == 'hi'
	}
	println('ok')
}
") or {
		panic(err)
	}

	bin := os.join_path(root, 'v3_ierror_str_codegen_input')
	compile := os.exec([@VEXE, '-nocache', '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('IError__str(err)'), c_code
	assert c_code.contains('return IError__str(err);'), c_code
	assert !c_code.contains('string__eq(err.message'), c_code
	// The interface method still receives the address of the typed receiver.
	assert c_code.contains('IError__msg(&err)'), c_code
	assert c_code.contains('err.code'), c_code

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}
