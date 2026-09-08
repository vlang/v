import io
import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')
const hello_src = os.join_path(tests_dir, 'testdata', 'hello.v')

fn read_c_output_fifo(path string, result chan string) {
	mut file := os.open(path) or {
		result <- 'read failed: ${err.msg()}'
		return
	}
	data := io.read_all(reader: file) or {
		file.close()
		result <- 'read failed: ${err.msg()}'
		return
	}
	file.close()
	result <- data.bytestr()
}

// test_c_output_path_only_writes_c_file validates this v3 regression case.
fn test_c_output_path_only_writes_c_file() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c_output_only_test')
	build :=
		os.execute('${vexe} -old-compiler -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	c_out := os.join_path(os.temp_dir(), 'v3_output_only.c')
	bin_out := c_out.all_before_last('.c')
	os.rm(c_out) or {}
	os.rm(bin_out) or {}

	mut compile := os.Result{}
	// Repeated no-GC runs exercise the checker struct-field pointer microcache.
	// Its pointer-to-integer field assignment used to be lowered as a managed
	// closure temporary, making this cgen path crash intermittently.
	for _ in 0 .. 16 {
		compile = os.execute('${v3_bin} -gc none -o ${c_out} ${hello_src}')
		assert compile.exit_code == 0, compile.output
	}
	assert os.exists(c_out)
	assert !os.exists(bin_out)
	assert compile.output.contains('cgen')
	assert !compile.output.contains('  > ')
	assert !compile.output.contains('tcc.exe')
	assert !compile.output.contains('cc -std=gnu11')

	$if !windows {
		fifo_out := os.join_path(os.temp_dir(), 'v3_output_fifo_${os.getpid()}.c')
		os.rm(fifo_out) or {}
		defer {
			os.rm(fifo_out) or {}
		}
		mkfifo := os.execute('mkfifo ${os.quoted_path(fifo_out)}')
		assert mkfifo.exit_code == 0, mkfifo.output
		fifo_result := chan string{cap: 1}
		reader := spawn read_c_output_fifo(fifo_out, fifo_result)
		fifo_compile := os.execute('${v3_bin} -gc none -o ${os.quoted_path(fifo_out)} ${hello_src}')
		fifo_source := <-fifo_result
		reader.wait()
		assert fifo_compile.exit_code == 0, fifo_compile.output
		assert !fifo_source.starts_with('read failed:'), fifo_source
		assert fifo_source.contains('int main(int argc, char** argv)'), fifo_source
	}
}
