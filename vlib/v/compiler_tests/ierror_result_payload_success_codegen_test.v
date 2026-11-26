import os
import v.tempname

fn ierror_result_c_body(c_code string, header string) string {
	start := c_code.index(header) or { return '' }
	tail := c_code[start..]
	end := tail.index('\n}\n') or { return tail }
	return tail[..end + 3]
}

fn test_ierror_shaped_result_payload_returns_success_before_error_boxing() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_result_payload_success_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'main.v')
	os.write_file(src, "struct MyErr {
	label string
}

fn (err MyErr) msg() string {
	return err.label
}

fn (err MyErr) code() int {
	return 7
}

fn payload() !MyErr {
	return MyErr{
		label: 'payload'
	}
}

fn payload_defer() !MyErr {
	defer {
		assert true
	}
	return MyErr{
		label: 'payload-defer'
	}
}

fn payload_ierror() !IError {
	return MyErr{
		label: 'payload-ierror'
	}
}

fn payload_ierror_local_err() !IError {
	err := MyErr{
		label: 'payload-local-err'
	}
	return err
}

fn payload_ierror_defer() !IError {
	defer {
		assert true
	}
	return MyErr{
		label: 'payload-ierror-defer'
	}
}

fn fail() !int {
	return MyErr{
		label: 'real-error'
	}
}

fn fail_fixed() ![2]int {
	return MyErr{
		label: 'fixed-error'
	}
}

fn make_err() IError {
	return error('boom')
}

fn no_ierror() ?IError {
	return none
}

fn fallback_ierror() !IError {
	return no_ierror() or {
		MyErr{
			label: 'fallback-ierror'
		}
	}
}

fn fail_ierror_payload() !IError {
	return error('payload-error')
}

fn fail_interface() !int {
	return make_err()
}

fn fail_interface_defer() !int {
	defer {
		assert true
	}
	return make_err()
}

fn main() {
	x := payload() or {
		println('ERR:' + err.msg())
		return
	}
	println('OK:' + x.msg())

	y := payload_defer() or {
		println('ERR_DEFER:' + err.msg())
		return
	}
	println('OK:' + y.msg())

	i := payload_ierror() or {
		println('IERR_PAYLOAD_ERR:' + err.msg())
		return
	}
	println('IERR_OK:' + i.msg())

	local_err := payload_ierror_local_err() or {
		println('IERR_LOCAL_ERR:' + err.msg())
		return
	}
	println('IERR_LOCAL_OK:' + local_err.msg())

	j := payload_ierror_defer() or {
		println('IERR_DEFER_PAYLOAD_ERR:' + err.msg())
		return
	}
	println('IERR_DEFER_OK:' + j.msg())

	k := fallback_ierror() or {
		println('IERR_FALLBACK_ERR:' + err.msg())
		return
	}
	println('IERR_FALLBACK_OK:' + k.msg())

	fail_ierror_payload() or {
		println('IERR_PAYLOAD_FAILURE:' + err.msg())
	}

	fail() or {
		println('ERR:' + err.msg() + ':' + err.code().str())
	}

	fail_fixed() or {
		println('FIXED_ERR:' + err.msg())
	}

	fail_interface() or {
		println('IERR:' + err.msg())
	}

	fail_interface_defer() or {
		println('IERR_DEFER:' + err.msg())
		return
	}
	println('BAD:success')
}
") or {
		panic(err)
	}

	bin := os.join_path(root, 'v3_ierror_result_payload_success_out')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'OK:payload\nOK:payload-defer\nIERR_OK:payload-ierror\nIERR_LOCAL_OK:payload-local-err\nIERR_DEFER_OK:payload-ierror-defer\nIERR_FALLBACK_OK:fallback-ierror\nIERR_PAYLOAD_FAILURE:payload-error\nERR:real-error:7\nFIXED_ERR:fixed-error\nIERR:boom\nIERR_DEFER:boom'

	c_code := os.read_file(bin + '.c') or { panic(err) }
	payload_ierror_body := ierror_result_c_body(c_code, '__v_result_IError payload_ierror(void) {')
	assert payload_ierror_body.contains('.ok = true, .value = (IError){._typ = '), payload_ierror_body
	assert !payload_ierror_body.contains('.ok = false'), payload_ierror_body

	payload_ierror_local_body := ierror_result_c_body(c_code,
		'__v_result_IError payload_ierror_local_err(void) {')
	assert payload_ierror_local_body.contains('.ok = true, .value = (IError){._typ = '), payload_ierror_local_body
	assert payload_ierror_local_body.contains('memdup((main__MyErr[]){\terr}, sizeof(main__MyErr))'), payload_ierror_local_body
	assert !payload_ierror_local_body.contains('.ok = false'), payload_ierror_local_body

	payload_ierror_defer_body := ierror_result_c_body(c_code, '__v_result_IError payload_ierror_defer(void) {')
	assert payload_ierror_defer_body.contains('= (__v_result_IError){.ok = true, .value = '), payload_ierror_defer_body

	assert payload_ierror_defer_body.contains('(IError){._typ = '), payload_ierror_defer_body
	assert !payload_ierror_defer_body.contains('= (__v_result_IError){.ok = false'), payload_ierror_defer_body
}
