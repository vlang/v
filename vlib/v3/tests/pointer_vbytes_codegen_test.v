import os

const pointer_vbytes_vexe = @VEXE
const pointer_vbytes_tests_dir = os.dir(@FILE)
const pointer_vbytes_v3_dir = os.dir(pointer_vbytes_tests_dir)
const pointer_vbytes_vlib_dir = os.dir(pointer_vbytes_v3_dir)
const pointer_vbytes_v3_src = os.join_path(pointer_vbytes_v3_dir, 'v3.v')

fn pointer_vbytes_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_pointer_vbytes_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${pointer_vbytes_vexe} -gc none -path "${pointer_vbytes_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${pointer_vbytes_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn pointer_vbytes_compact_c(c_code string) string {
	return c_code.replace(' ', '').replace('\t', '')
}

fn test_u8_pointer_vbytes_uses_byteptr_helper() {
	v3_bin := pointer_vbytes_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_pointer_vbytes_input_${os.getpid()}.v')
	os.write_file(src, "
fn main() {
	unsafe {
		bp := malloc(5)
		bp[0] = 1
		bp[1] = 2
		bp[2] = 3
		bp[3] = 4
		bp[4] = 255
		bytes := bp.vbytes(5)
		prefix := voidptr(bp).vbytes(3)
		assert bytes.len == 5
		assert bytes[0] == u8(1)
		assert bytes[4] == u8(255)
		assert prefix.len == 3
		assert prefix[2] == u8(3)
		println('ok')
	}
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_pointer_vbytes_input_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	c_code := os.read_file(bin + '.c') or { panic(err) }
	c_compact := pointer_vbytes_compact_c(c_code)
	assert c_code.contains('Array byteptr__vbytes('), c_code
	assert c_code.contains('Array voidptr__vbytes('), c_code
	assert c_compact.contains('byteptr__vbytes(bp,5)'), c_code
	assert c_compact.contains('voidptr__vbytes((void*)(bp),3)')
		|| c_compact.contains('voidptr__vbytes((void*)bp,3)'), c_code
	assert !c_code.contains('u8__vbytes'), c_code
}
