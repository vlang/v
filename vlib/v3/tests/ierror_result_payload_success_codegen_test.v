import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_ierror_result_payload_success_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_ierror_shaped_result_payload_returns_success_before_error_boxing() {
	v3_bin := build_v3()

	root := os.join_path(os.temp_dir(), 'v3_ierror_result_payload_success_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
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

fn fail() !int {
	return MyErr{
		label: 'real-error'
	}
}

fn make_err() IError {
	return error('boom')
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

	fail() or {
		println('ERR:' + err.msg() + ':' + err.code().str())
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

	bin := os.join_path(os.temp_dir(), 'v3_ierror_result_payload_success_out_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'OK:payload\nOK:payload-defer\nERR:real-error:7\nIERR:boom\nIERR_DEFER:boom'
}
