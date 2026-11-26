import os
import v.tempname

fn pointer_vbytes_compact_c(c_code string) string {
	return c_code.replace(' ', '').replace('\t', '')
}

fn test_u8_pointer_vbytes_uses_byteptr_helper() {
	dir := os.join_path(os.vtmp_dir(), 'v3_pointer_vbytes_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	bin := os.join_path(dir, 'v3_pointer_vbytes_input')
	src := bin + '.v'
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

	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
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
