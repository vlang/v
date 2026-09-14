// Portable `-os cross` C must carry the bytes of every `$embed_file`.
//
// An ordinary debug build only records the file's absolute path and re-reads it the
// first time `.data()` is called, which keeps rebuilds cheap. A portable snapshot is
// compiled and run on a different machine, where that path names nothing, so the same
// trick turns into a startup panic:
//
//	V panic: EmbedFileData error: files "manual_stdlib_c_headers.h" and
//	"/home/runner/work/v/v/vlib/v/gen/c/manual_stdlib_c_headers.h" do not exist
//
// That is exactly how `vc/v.c` broke `make`: the compiler embeds its own C header
// prelude, and the published snapshot pointed at the CI runner's checkout.
import os

const tricky_payload = 'quote:" backslash:\\ trigraph:??! nul:\x00 high:\xfe\xff\nsecond line\n'

fn cross_compile_probe() ?(string, string) {
	vexe := os.getenv('VEXE')
	if vexe == '' {
		return none
	}
	dir := os.join_path(os.vtmp_dir(), 'embed_file_cross_${os.getpid()}')
	os.mkdir_all(dir) or { return none }
	os.write_file(os.join_path(dir, 'payload.bin'), tricky_payload) or { return none }
	src := os.join_path(dir, 'prog.v')
	os.write_file(src, "module main

const payload = \$embed_file('payload.bin').to_bytes()

fn main() {
	mut sum := u32(0)
	for b in payload {
		sum = sum * 31 + u32(b)
	}
	println('\${payload.len} \${sum}')
}
") or { return none }
	out := os.join_path(dir, 'prog.c')
	res := os.execute('${os.quoted_path(vexe)} -os cross -o ${os.quoted_path(out)} ${os.quoted_path(src)}')
	if res.exit_code != 0 {
		assert false, res.output
	}
	return dir, out
}

fn expected_output() string {
	mut sum := u32(0)
	for b in tricky_payload.bytes() {
		sum = sum * 31 + u32(b)
	}
	return '${tricky_payload.len} ${sum}'
}

fn test_cross_output_embeds_the_file_contents() {
	dir, out := cross_compile_probe() or { return }
	defer {
		os.rmdir_all(dir) or {}
	}
	generated := os.read_file(out) or {
		assert false, err.msg()
		return
	}
	// The payload is materialized as a C string literal, so no code path in the
	// snapshot has to find `payload.bin` again.
	assert generated.contains('.uncompressed = (u8*)"'), 'the embedded bytes are missing from the portable snapshot'
}

fn test_cross_snapshot_runs_without_the_embedded_file() {
	$if windows {
		// The snapshot is built here with a plain `cc` invocation, which is not how
		// the Windows toolchains are driven.
		return
	}
	cc := os.find_abs_path_of_executable('cc') or { return }
	dir, out := cross_compile_probe() or { return }
	defer {
		os.rmdir_all(dir) or {}
	}
	exe := os.join_path(dir, 'prog')
	build := os.execute('${os.quoted_path(cc)} -std=gnu11 -w -o ${os.quoted_path(exe)} ${os.quoted_path(out)} -lm -lpthread')
	assert build.exit_code == 0, build.output
	// Stand in for "compiled on another machine": the file the snapshot was
	// generated from is gone by the time the program runs.
	os.rm(os.join_path(dir, 'payload.bin')) or {
		assert false, err.msg()
		return
	}
	res := os.execute(os.quoted_path(exe))
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == expected_output()
}
