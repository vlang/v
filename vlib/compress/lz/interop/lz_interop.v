module main

import compress.lz
import os

const default_data_size = 512 * 1024

fn main() {
	mut data_size := default_data_size
	if os.args.len > 1 {
		parsed := int(os.args[1].i32())
		if parsed > 0 {
			data_size = parsed
		}
	}
	data := `z`.repeat(data_size).bytes()

	println('LZ interop input: ${data.len} bytes')

	for format in [lz.Format.lz77, .lz78, .lzw, .lz4, .lzss, .lzma, .lzma2, .lzjb] {
		validate_v_roundtrip(data, format) or {
			eprintln('V validation failed for ${format}: ${err.msg()}')
			exit(1)
		}
		println('V roundtrip (${format}): OK')
	}

	tmp_dir := os.join_path(os.temp_dir(), 'v_lz_interop')
	os.mkdir_all(tmp_dir) or {
		eprintln('Could not create temp directory ${tmp_dir}: ${err.msg()}')
		exit(1)
	}
	defer {
		os.rmdir_all(tmp_dir) or {
			eprintln('Could not remove temp directory ${tmp_dir}: ${err.msg()}')
		}
	}
	input_path := os.join_path(tmp_dir, 'input.bin')
	os.write_file_array(input_path, data) or {
		eprintln('Could not write input file ${input_path}: ${err.msg()}')
		exit(1)
	}
	mut c_bin := ''
	if bin := compile_c_runner() {
		c_bin = bin
	} else {
		eprintln('Skipping C benchmark: ${err.msg()}')
	}
	python_ok := has_python3()
	if !python_ok {
		eprintln('Skipping Python benchmark: python3 is not available')
	}

	if c_bin.len > 0 {
		cross_validate_v_c(c_bin, data, input_path, tmp_dir) or {
			eprintln('Cross-validation V<->C failed: ${err.msg()}')
			exit(1)
		}
		println('Cross-validation: V<->C compress/decompress OK')
	} else {
		println('Cross-validation: skipped V<->C (requires C compiler)')
	}

	if python_ok {
		cross_validate_v_python(data, input_path, tmp_dir) or {
			eprintln('Cross-validation V<->Python failed: ${err.msg()}')
			exit(1)
		}
		println('Cross-validation: V<->Python compress/decompress OK')
	} else {
		println('Cross-validation: skipped V<->Python (requires python3)')
	}
}

fn validate_v_roundtrip(data []u8, format lz.Format) ! {
	encoded := lz.compress(data, format)!
	decoded := lz.decompress(encoded, format)!
	if decoded != data {
		return error('roundtrip mismatch for ${format}')
	}
}

fn compile_c_runner() !string {
	cc := choose_cc()
	if cc.len == 0 {
		return error('no C compiler found (tried cc, gcc, and clang)')
	}
	bin_path := os.join_path(os.temp_dir(), 'lz77_ref_bench')
	c_src := os.join_path(@DIR, 'lz77_ref.c')
	compile_cmd := '${cc} -O3 -std=c99 ${os.quoted_path(c_src)} -o ${os.quoted_path(bin_path)}'
	compile_res := os.execute(compile_cmd)
	if compile_res.exit_code != 0 {
		return error('C compile failed: ${compile_res.output.trim_space()}')
	}
	return bin_path
}

fn choose_cc() string {
	for cc in ['cc', 'gcc', 'clang'] {
		if os.execute('${cc} --version').exit_code == 0 {
			return cc
		}
	}
	return ''
}

fn has_python3() bool {
	return os.execute('python3 --version').exit_code == 0
}

fn cross_validate_v_c(c_bin string, original []u8, input_path string, tmp_dir string) ! {
	v_encoded := os.join_path(tmp_dir, 'v_encoded.bin')
	c_decoded := os.join_path(tmp_dir, 'c_decoded.bin')
	c_encoded := os.join_path(tmp_dir, 'c_encoded.bin')

	v_stream := lz.compress_lz77(original)!
	os.write_file_array(v_encoded, v_stream)!

	mut res :=
		os.execute('${os.quoted_path(c_bin)} decompress ${os.quoted_path(v_encoded)} ${os.quoted_path(c_decoded)}')
	if res.exit_code != 0 {
		return error('C decompress(V output) failed: ${res.output.trim_space()}')
	}
	validate_equal_files(input_path, c_decoded, 'V->C')!

	res =
		os.execute('${os.quoted_path(c_bin)} compress ${os.quoted_path(input_path)} ${os.quoted_path(c_encoded)}')
	if res.exit_code != 0 {
		return error('C compress failed: ${res.output.trim_space()}')
	}
	c_encoded_data := os.read_bytes(c_encoded)!
	v_decoded := lz.decompress_lz77(c_encoded_data)!
	if v_decoded != original {
		return error('C->V output mismatch')
	}
}

fn cross_validate_v_python(original []u8, input_path string, tmp_dir string) ! {
	v_encoded := os.join_path(tmp_dir, 'v_encoded_for_py.bin')
	py_decoded := os.join_path(tmp_dir, 'py_decoded.bin')
	py_encoded := os.join_path(tmp_dir, 'py_encoded.bin')
	py_script := os.join_path(@DIR, 'lz77_ref.py')

	v_stream := lz.compress_lz77(original)!
	os.write_file_array(v_encoded, v_stream)!

	mut res :=
		os.execute('python3 ${os.quoted_path(py_script)} decompress ${os.quoted_path(v_encoded)} ${os.quoted_path(py_decoded)}')
	if res.exit_code != 0 {
		return error('Python decompress(V output) failed: ${res.output.trim_space()}')
	}
	validate_equal_files(input_path, py_decoded, 'V->Python')!

	res =
		os.execute('python3 ${os.quoted_path(py_script)} compress ${os.quoted_path(input_path)} ${os.quoted_path(py_encoded)}')
	if res.exit_code != 0 {
		return error('Python compress failed: ${res.output.trim_space()}')
	}
	py_encoded_data := os.read_bytes(py_encoded)!
	v_decoded := lz.decompress_lz77(py_encoded_data)!
	if v_decoded != original {
		return error('Python->V output mismatch')
	}
}

fn validate_equal_files(expected_path string, actual_path string, tag string) ! {
	expected := os.read_bytes(expected_path)!
	actual := os.read_bytes(actual_path)!
	if expected != actual {
		return error('${tag} output mismatch')
	}
}
