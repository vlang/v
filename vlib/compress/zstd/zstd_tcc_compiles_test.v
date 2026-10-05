import os
import rand

const vexe = @VEXE

const zstd_program = '
import compress.zstd

fn main() {
	data := "hello zstd from tcc ".repeat(200).bytes()
	compressed := zstd.compress(data)!
	assert compressed.len < data.len
	decompressed := zstd.decompress(compressed)!
	assert decompressed == data
	println("ok")
}
'

// Regression test for https://github.com/vlang/v/issues/29357 : tcc predefines
// `__GNUC__ 4` but has no `__builtin_prefetch`, so the bundled zstd failed to
// link with tcc on non-ARM hosts. The failure was hidden by the automatic
// fallback to the system C compiler, hence `-no-retry-compilation` here.
fn test_compress_zstd_compiles_and_runs_with_tcc() {
	$if !((linux || macos) && (amd64 || arm64)) {
		return
	}
	workdir := os.join_path(os.vtmp_dir(), 'v_zstd_tcc_${rand.ulid()}')
	os.mkdir_all(workdir) or { panic(err) }
	defer {
		os.rmdir_all(workdir) or {}
	}
	vcache := os.join_path(workdir, 'vcache')
	os.mkdir_all(vcache) or { panic(err) }
	src := os.join_path(workdir, 'main.v')
	out := os.join_path(workdir, 'main')
	os.write_file(src, zstd_program) or { panic(err) }
	res := os.exec(['env', 'VCACHE=${vcache}', vexe, '-nocache', '-cc', 'tcc', '-gc', 'none',
		'-no-retry-compilation', '-o', out, src])
	if res.exit_code != 0 {
		panic('tcc compilation of a compress.zstd program failed (fallback is disabled):\n${res.output}')
	}
	run_res := os.exec([out])
	assert run_res.exit_code == 0, run_res.output
	assert run_res.output.trim_space() == 'ok'
}
