import io
import os
import v.tempname

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
	root := os.join_path(os.vtmp_dir(), 'v3_c_output_only_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	hello_src := os.join_path(os.dir(@FILE), 'testdata', 'hello.v')
	bin_out := os.join_path(root, 'v3_output_only')
	c_out := bin_out + '.c'

	mut compile := os.Result{}
	// Repeated no-GC runs exercise the checker struct-field pointer microcache.
	// Its pointer-to-integer field assignment used to be lowered as a managed
	// closure temporary, making this cgen path crash intermittently.
	for _ in 0 .. 16 {
		compile = os.exec([@VEXE, '-gc', 'none', '-o', c_out, hello_src])
		assert compile.exit_code == 0, compile.output
	}
	assert os.exists(c_out)
	assert !os.exists(bin_out)
	assert !compile.output.contains('  > ')
	assert !compile.output.contains('tcc.exe')
	assert !compile.output.contains('cc -std=gnu11')
	// A C-only build is silent, so the pipeline has to be observed with `-v`: it
	// must reach cgen and stop there, without handing anything to a C compiler.
	os.rm(c_out)!
	verbose_compile := os.exec([@VEXE, '-v', '-gc', 'none', '-o', c_out, hello_src])
	assert verbose_compile.exit_code == 0, verbose_compile.output
	assert os.exists(c_out)
	assert !os.exists(bin_out)
	assert verbose_compile.output.contains('cgen'), verbose_compile.output
	assert !verbose_compile.output.contains('tcc.exe'), verbose_compile.output
	assert !verbose_compile.output.contains('cc -std=gnu11'), verbose_compile.output

	$if !windows {
		fifo_out := os.join_path(root, 'v3_output_fifo.c')
		mkfifo := os.exec(['mkfifo', fifo_out])
		assert mkfifo.exit_code == 0, mkfifo.output
		fifo_result := chan string{cap: 1}
		reader := spawn read_c_output_fifo(fifo_out, fifo_result)
		fifo_compile := os.exec([@VEXE, '-gc', 'none', '-o', fifo_out, hello_src])
		fifo_source := <-fifo_result
		reader.wait()
		assert fifo_compile.exit_code == 0, fifo_compile.output
		assert !fifo_source.starts_with('read failed:'), fifo_source
		regular_source := os.read_file(c_out) or { panic(err) }
		assert fifo_source == regular_source, fifo_source
		assert !os.exists(bin_out)
		assert !fifo_compile.output.contains('tcc.exe'), fifo_compile.output
		assert !fifo_compile.output.contains('cc -std=gnu11'), fifo_compile.output
	}
}
