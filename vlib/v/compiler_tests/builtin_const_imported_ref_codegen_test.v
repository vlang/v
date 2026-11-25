import os
import v.tempname

fn test_builtin_const_ref_from_imported_module_is_qualified() {
	dir := os.join_path(os.vtmp_dir(), 'v3_builtin_const_imported_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	bin := os.join_path(dir, 'v3_builtin_const_imported_input')
	src := bin + '.v'
	os.write_file(src, "import strconv

fn main() {
	n, err := strconv.common_parse_uint2('18446744073709551615', 10, 64)
	assert err == 0
	assert n == max_u64
	println('ok')
}
") or {
		panic(err)
	}

	// This test inspects the imported implementation in the monolithic C output.
	compile := os.exec([@VEXE, '-nocache', src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('#define builtin__max_u64'), c_code
	assert c_code.contains('= (u64)(builtin__max_u64);'), c_code
	assert c_code.contains('= (u64)((u64)(base));'), c_code
	assert c_code.contains('v_panic(_S("division by zero"))'), c_code
	// `if bit_size == 64 { max_u64 } else { ... }` is a scalar select, emitted as a conditional.
	assert c_code.contains('? builtin__max_u64 :'), c_code
	assert !c_code.contains('(max_u64 /'), c_code
	assert !c_code.contains('= max_u64;'), c_code
}
