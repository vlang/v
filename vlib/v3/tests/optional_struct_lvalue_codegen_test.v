import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn test_optional_struct_selector_assignment_mutates_payload_storage() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_optional_struct_lvalue_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_optional_struct_lvalue_input.v')
	os.write_file(src, "struct AFoo {
mut:
	name string
}

struct Inner {
mut:
	name string
}

struct Holder {
mut:
	opt ?Inner
}

struct ResultHolder {
mut:
	res !Inner
}

struct Outer {
mut:
	inner Inner
}

fn (mut f AFoo) opt_string(arr ?[]int) ?string {
	return arr?.len.str()
}

fn guarded_failure() ?AFoo {
	mut m := ?AFoo(none)
	m?.name = 'bad'
	return m
}

fn fail_inner() !Inner {
	return error('boom')
}

fn mutate_result_source(mut h ResultHolder) ! {
	h.res!.name = 'result'
}

fn main() {
	mut m := ?AFoo(AFoo{})
	assert m?.opt_string([1, 2, 3])? == '3'
	m?.name = 'foo'
	assert m?.name == 'foo'
	mut holder := Holder{
		opt: Inner{}
	}
	holder.opt?.name = 'holder'
	assert holder.opt?.name == 'holder'
	mut outer := ?Outer(Outer{})
	outer?.inner.name = 'deep'
	assert outer?.inner.name == 'deep'
	mut result_ok := ResultHolder{
		res: Inner{}
	}
	mutate_result_source(mut result_ok)!
	assert result_ok.res!.name == 'result'
	mut result_bad := ResultHolder{
		res: fail_inner()
	}
	mut saw_result_err := false
	mutate_result_source(mut result_bad) or {
		assert err.msg() == 'boom'
		saw_result_err = true
	}
	assert saw_result_err
	guarded_failure() or {
		assert err.msg() == ''
		println('ok')
		return
	}
	assert false
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_optional_struct_lvalue_input')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('m.value.name ='), c_code
	assert c_code.contains('holder.opt.value.name ='), c_code
	assert c_code.contains('outer.value.inner.name ='), c_code
	assert c_code.contains('if (!m.ok)'), c_code
	assert c_code.contains('if (!holder.opt.ok)'), c_code
	assert c_code.contains('if (!outer.ok)'), c_code
	assert c_code.contains('.err = h->res.err') || c_code.contains('.err = h.res.err'), c_code
	mutate_result_start := c_code.index('\nOptional mutate_result_source(ResultHolder* h) {') or {
		-1
	}
	assert mutate_result_start >= 0, c_code
	mutate_result_tail := c_code[mutate_result_start..]
	mutate_result_guard := mutate_result_tail[..mutate_result_tail.index('h->res.value.name') or {
		mutate_result_tail.len
	}]
	assert !mutate_result_guard.contains('return (Optional){.ok = false};'), mutate_result_guard
	for line in c_code.split_into_lines() {
		assert !(line.contains('__or_val_') && line.contains('.name =')), line
		assert !(line.contains('__or_val_') && line.contains('.inner.name =')), line
	}

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}
