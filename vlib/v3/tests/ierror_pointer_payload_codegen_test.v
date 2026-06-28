import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_ierror_pointer_payload_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn c_fn_body(c_code string, header string) string {
	start := c_code.index(header) or { return '' }
	tail := c_code[start..]
	end := tail.index('\n}\n') or { return tail }
	return tail[..end + 3]
}

fn test_local_pointer_ierror_payload_is_heap_copied() {
	v3_bin := build_v3()

	root := os.join_path(os.temp_dir(), 'v3_ierror_local_pointer_payload_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	src := os.join_path(root, 'main.v')
	os.write_file(src, "struct PtrErr {
	text string
}

fn (err &PtrErr) msg() string {
	return err.text
}

fn (err &PtrErr) code() int {
	return 7
}

fn local_ptr() !int {
	err := PtrErr{
		text: 'stack error'
	}
	return &err
}

fn direct_local() IError {
	err := PtrErr{
		text: 'direct error'
	}
	return &err
}

fn from_param(err PtrErr) !int {
	return &err
}

fn main() {
	local_ptr() or {
		assert err.msg() == 'stack error'
		assert err.code() == 7
	}
	direct := direct_local()
	assert direct.msg() == 'direct error'
	assert direct.code() == 7
	from_param(PtrErr{
		text: 'param error'
	}) or {
		assert err.msg() == 'param error'
		assert err.code() == 7
	}
	println('ok')
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_ierror_local_pointer_payload_out_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	c_code := os.read_file(bin + '.c') or { panic(err) }
	local_body := c_fn_body(c_code, 'Optional local_ptr(void) {')
	assert local_body.contains('._object = memdup('), local_body
	assert local_body.contains('sizeof(PtrErr)'), local_body
	assert !local_body.contains('._object = &err'), local_body

	direct_body := c_fn_body(c_code, 'IError direct_local(void) {')
	assert direct_body.contains('._object = memdup('), direct_body
	assert direct_body.contains('sizeof(PtrErr)'), direct_body
	assert !direct_body.contains('._object = &err'), direct_body

	param_body := c_fn_body(c_code, 'Optional from_param(PtrErr err) {')
	assert param_body.contains('._object = memdup('), param_body
	assert param_body.contains('sizeof(PtrErr)'), param_body
	assert !param_body.contains('._object = &err'), param_body
}
