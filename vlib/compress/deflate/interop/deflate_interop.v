module main

import compress.deflate
import os

fn choose_cc() string {
	for cc in ['cc', 'gcc', 'clang'] {
		if os.execute('${cc} --version').exit_code == 0 {
			return cc
		}
	}
	return ''
}

fn compile_c_ref(workdir string) !string {
	cc := choose_cc()
	if cc == '' {
		return error('no C compiler found')
	}
	src := os.join_path(@DIR, 'deflate_ref.c')
	bin := os.join_path(workdir, 'deflate_cross_validate')
	res := os.execute('${cc} -O2 ${os.quoted_path(src)} -lz -o ${os.quoted_path(bin)}')
	if res.exit_code != 0 {
		return error('C compile failed: ${res.output}')
	}
	return bin
}

fn main() {
	base_tmp := os.join_path(os.temp_dir(), 'v_deflate_interop')
	os.mkdir_all(base_tmp) or {
		eprintln('could not create base temp dir: ${err.msg()}')
		exit(1)
	}
	workdir := os.join_path(base_tmp, 'run_${os.getpid()}')
	os.mkdir_all(workdir) or {
		eprintln('could not create work dir: ${err.msg()}')
		exit(1)
	}
	defer {
		os.rmdir_all(workdir) or {}
	}

	bin := compile_c_ref(workdir) or {
		eprintln('Skipping C cross-validation: ${err.msg()}')
		return
	}

	input := 'deflate C cross-validation payload'.repeat(50).bytes()
	ip := os.join_path(workdir, 'xval_in.bin')
	cp := os.join_path(workdir, 'xval_c_zlib.bin')
	gp := os.join_path(workdir, 'xval_c_gzip.bin')
	vp := os.join_path(workdir, 'xval_v_zlib.bin')
	vgp := os.join_path(workdir, 'xval_v_gzip.bin')
	dp := os.join_path(workdir, 'xval_dec.bin')

	os.write_file_array(ip, input) or {
		eprintln('write input failed: ${err.msg()}')
		exit(1)
	}

	res1 :=
		os.execute('${os.quoted_path(bin)} compress ${os.quoted_path(ip)} ${os.quoted_path(cp)}')
	if res1.exit_code != 0 {
		eprintln('C zlib compress failed: ${res1.output}')
		exit(1)
	}
	c_zlib := os.read_bytes(cp) or {
		eprintln('read C zlib stream failed: ${err.msg()}')
		exit(1)
	}
	v_decoded_zlib := deflate.decompress(c_zlib) or {
		eprintln('V decompress of C zlib failed: ${err.msg()}')
		exit(1)
	}
	if v_decoded_zlib != input {
		eprintln('C zlib -> V mismatch')
		exit(1)
	}
	println('OK: C zlib -> V decompress')

	res2 := os.execute('${os.quoted_path(bin)} gzip ${os.quoted_path(ip)} ${os.quoted_path(gp)}')
	if res2.exit_code != 0 {
		eprintln('C gzip failed: ${res2.output}')
		exit(1)
	}
	c_gzip := os.read_bytes(gp) or {
		eprintln('read C gzip stream failed: ${err.msg()}')
		exit(1)
	}
	v_decoded_gzip := deflate.decompress(c_gzip) or {
		eprintln('V decompress of C gzip failed: ${err.msg()}')
		exit(1)
	}
	if v_decoded_gzip != input {
		eprintln('C gzip -> V mismatch')
		exit(1)
	}
	println('OK: C gzip -> V decompress')

	v_zlib := deflate.compress(input) or {
		eprintln('V zlib compress failed: ${err.msg()}')
		exit(1)
	}
	os.write_file_array(vp, v_zlib) or {
		eprintln('write V zlib failed: ${err.msg()}')
		exit(1)
	}
	res3 :=
		os.execute('${os.quoted_path(bin)} decompress ${os.quoted_path(vp)} ${os.quoted_path(dp)}')
	if res3.exit_code != 0 {
		eprintln('C decompress of V zlib failed: ${res3.output}')
		exit(1)
	}
	c_unzlib := os.read_bytes(dp) or {
		eprintln('read C zlib decompressed output failed: ${err.msg()}')
		exit(1)
	}
	if c_unzlib != input {
		eprintln('V zlib -> C mismatch')
		exit(1)
	}
	println('OK: V zlib -> C decompress')

	v_gzip := deflate.compress(input, format: .gzip) or {
		eprintln('V gzip compress failed: ${err.msg()}')
		exit(1)
	}
	os.write_file_array(vgp, v_gzip) or {
		eprintln('write V gzip failed: ${err.msg()}')
		exit(1)
	}
	res4 := os.execute('${os.quoted_path(bin)} gunzip ${os.quoted_path(vgp)} ${os.quoted_path(dp)}')
	if res4.exit_code != 0 {
		eprintln('C gunzip of V gzip failed: ${res4.output}')
		exit(1)
	}
	c_ungzip := os.read_bytes(dp) or {
		eprintln('read C gzip decompressed output failed: ${err.msg()}')
		exit(1)
	}
	if c_ungzip != input {
		eprintln('V gzip -> C mismatch')
		exit(1)
	}
	println('OK: V gzip -> C decompress')

	if os.execute('python3 --version').exit_code == 0 {
		py_src := os.join_path(@DIR, 'deflate_ref.py')
		py_driver := os.join_path(workdir, 'deflate_ref.py')
		os.cp(py_src, py_driver) or {
			eprintln('copy Python reference script failed: ${err.msg()}')
			exit(1)
		}

		py_zp := os.join_path(workdir, 'xval_py_zlib.bin')
		py_gp := os.join_path(workdir, 'xval_py_gzip.bin')

		res5 :=
			os.execute('python3 ${os.quoted_path(py_driver)} compress ${os.quoted_path(ip)} ${os.quoted_path(py_zp)}')
		if res5.exit_code != 0 {
			eprintln('Python zlib compress failed: ${res5.output}')
			exit(1)
		}
		py_zlib := os.read_bytes(py_zp) or {
			eprintln('read Python zlib stream failed: ${err.msg()}')
			exit(1)
		}
		if deflate.decompress(py_zlib) or { []u8{} } != input {
			eprintln('Python zlib -> V mismatch')
			exit(1)
		}
		println('OK: Python zlib -> V decompress')

		res6 :=
			os.execute('python3 ${os.quoted_path(py_driver)} gzip ${os.quoted_path(ip)} ${os.quoted_path(py_gp)}')
		if res6.exit_code != 0 {
			eprintln('Python gzip failed: ${res6.output}')
			exit(1)
		}
		py_gzip := os.read_bytes(py_gp) or {
			eprintln('read Python gzip stream failed: ${err.msg()}')
			exit(1)
		}
		if deflate.decompress(py_gzip) or { []u8{} } != input {
			eprintln('Python gzip -> V mismatch')
			exit(1)
		}
		println('OK: Python gzip -> V decompress')

		py_unz := os.join_path(workdir, 'xval_py_unz.bin')
		res7 :=
			os.execute('python3 ${os.quoted_path(py_driver)} decompress ${os.quoted_path(vp)} ${os.quoted_path(py_unz)}')
		if res7.exit_code != 0 {
			eprintln('Python decompress of V zlib failed: ${res7.output}')
			exit(1)
		}
		if (os.read_bytes(py_unz) or { []u8{} }) != input {
			eprintln('V zlib -> Python mismatch')
			exit(1)
		}
		println('OK: V zlib -> Python decompress')

		py_ungz := os.join_path(workdir, 'xval_py_ungz.bin')
		res8 :=
			os.execute('python3 ${os.quoted_path(py_driver)} gunzip ${os.quoted_path(vgp)} ${os.quoted_path(py_ungz)}')
		if res8.exit_code != 0 {
			eprintln('Python gunzip of V gzip failed: ${res8.output}')
			exit(1)
		}
		if (os.read_bytes(py_ungz) or { []u8{} }) != input {
			eprintln('V gzip -> Python mismatch')
			exit(1)
		}
		println('OK: V gzip -> Python decompress')
	} else {
		eprintln('Skipping Python cross-validation: python3 not found')
	}
}
