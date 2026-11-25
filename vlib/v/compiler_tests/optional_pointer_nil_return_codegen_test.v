import os
import v.tempname

fn test_optional_pointer_nil_return_is_success_payload() {
	dir := os.join_path(os.vtmp_dir(), 'v3_optional_pointer_nil_return_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer { os.rmdir_all(dir) or {} }
	bin := os.join_path(dir, 'v3_optional_pointer_nil_return_input')
	src := bin + '.v'
	os.write_file(src, "struct T {
	x int
}

fn maybe(flag bool) ?&T {
	if flag {
		return unsafe { nil }
	}
	return none
}

fn maybe_defer(flag bool) ?&T {
	defer {}
	if flag {
		return unsafe { nil }
	}
	return none
}

fn result_maybe(flag bool) !&T {
	if flag {
		return unsafe { nil }
	}
	return error('no value')
}

fn result_maybe_defer(flag bool) !&T {
	defer {}
	if flag {
		return unsafe { nil }
	}
	return error('no value')
}

fn check_question_unwrap() ? {
	a := maybe(true)?
	assert a == unsafe { nil }
	c := maybe_defer(true)?
	assert c == unsafe { nil }
}

fn check_result_unwrap() ! {
	r := result_maybe(true)!
	assert r == unsafe { nil }
	rd := result_maybe_defer(true)!
	assert rd == unsafe { nil }
}

fn main() {
	check_question_unwrap() or { panic('missing value') }
	check_result_unwrap() or { panic(err) }
	b := maybe(false) or { unsafe { nil } }
	assert b == unsafe { nil }
	d := maybe_defer(false) or { unsafe { nil } }
	assert d == unsafe { nil }
	rb := result_maybe(false) or {
		assert err.msg() == 'no value'
		unsafe { nil }
	}
	assert rb == unsafe { nil }
	rbd := result_maybe_defer(false) or {
		assert err.msg() == 'no value'
		unsafe { nil }
	}
	assert rbd == unsafe { nil }
	println('ok')
}
") or {
		panic(err)
	}

	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('return (__v_option_main__Tptr){.ok = true, .value = NULL};'), c_code
	assert c_code.contains('__v_option_main__Tptr _t')
		&& c_code.contains('= (__v_option_main__Tptr){.ok = true, .value =')
		&& c_code.contains('NULL};'), c_code

	assert c_code.contains('return (__v_option_main__Tptr){.ok = false};'), c_code
	assert c_code.contains('return (__v_result_main__Tptr){.ok = false, .err = (IError)'), c_code

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}
