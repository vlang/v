import os

const vexe = @VEXE

struct MipsClosureResult {
	output    string
	exit_code int
	c_source  string
}

fn compile_closure_probe_for_arch(name string, arch string, source string) MipsClosureResult {
	dir := os.join_path(os.vtmp_dir(), 'v3_mips64le_closure_${name}_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	src := os.join_path(dir, 'probe.v')
	os.write_file(src, source) or { panic(err) }
	out := os.join_path(dir, 'probe.c')
	res := os.exec([vexe, '-new-compiler', '-nocache', '-os', 'linux', '-arch', arch, '-o', out,
		src])
	return MipsClosureResult{
		output:    res.output
		exit_code: res.exit_code
		c_source:  os.read_file(out) or { '' }
	}
}

fn test_mips64le_rejects_capturing_fn_literal() {
	res := compile_closure_probe_for_arch('fn_literal', 'mips64le', 'module main

fn main() {
	value := 12
	callback := fn [value] () int {
		return value
	}
	println(callback())
}
')
	assert res.exit_code != 0, res.output
	assert res.output.contains('capturing closures are not supported'), res.output
	// The rejected build must never carry PowerPC instructions to mips64le.
	assert !res.c_source.contains('0x7c, 0x08, 0x02, 0xa6'), 'PPC thunk bytes reached mips64le C'
	assert !res.c_source.contains('0x94, 0x21, 0xff, 0xf0'), 'PPC get_data bytes reached mips64le C'
}

fn test_mips64le_rejects_method_value_closure() {
	res := compile_closure_probe_for_arch('method_value', 'mips64le', 'module main

struct Counter {
	value int
}

fn (c Counter) get() int {
	return c.value
}

fn main() {
	counter := Counter{ value: 7 }
	callback := counter.get
	println(callback())
}
')
	assert res.exit_code != 0, res.output
	assert res.output.contains('capturing closures are not supported'), res.output
}

fn test_mips64le_generates_c_without_ppc_thunk_bytes_without_closures() {
	res := compile_closure_probe_for_arch('plain', 'mips64le', 'module main

fn main() {
	println(42)
}
')
	assert res.exit_code == 0, res.output
	assert res.c_source.contains('closure__closure_thunk[1]'), res.c_source
	assert !res.c_source.contains('0x7c, 0x08, 0x02, 0xa6'), res.c_source
	assert !res.c_source.contains('0x94, 0x21, 0xff, 0xf0'), res.c_source
}

fn test_other_targets_still_emit_capturing_closures() {
	res := compile_closure_probe_for_arch('amd64', 'amd64', 'module main

fn main() {
	value := 12
	callback := fn [value] () int {
		return value
	}
	println(callback())
}
')
	assert res.exit_code == 0, res.output
	assert res.c_source.contains('closure__closure_thunk'), res.c_source
}
