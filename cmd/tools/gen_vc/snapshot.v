module main

import os

// A generated file shorter than this means that generation went wrong.
const snapshot_min_lines = 5000

// snapshot_files maps each file of a vlang/vc snapshot to the flags that generate it from cmd/v.
// `-cross` makes `$if cross ?` (and the `?`-guarded v1_fallback/musl branches that key off it)
// resolve statically instead of compiling out empty, which lets GNUmakefile recompile the snapshot
// with -DCUSTOM_DEFINE_v1_fallback to build v1.
const snapshot_files = {
	'v.c':     ['-cross']
	'v_win.c': ['-cross', '-os', 'windows', '-cc', 'msvc']
}

fn commit_hash_line(commit string) string {
	return '#define V_COMMIT_HASH "${commit}"\n'
}

// in_dir returns argv that runs `args` with `dir` as its working directory, without changing the
// working directory of this process, whose HTTP threads keep running during a build. VEXE and VFLAGS
// are dropped, so the compiler under `dir` builds with its own defaults.
fn in_dir(dir string, args []string) []string {
	mut command := ['env', '-u', 'VEXE', '-u', 'VFLAGS', 'sh', '-c', 'cd "\$1" && shift && exec "\$@"',
		'sh', dir]
	command << args
	return command
}

// generate_snapshot writes v.c and v_win.c of the already built V checkout `v_root` into `out_dir`.
// The first line of both is `#define V_COMMIT_HASH "<commit>"`, and both must pass check_snapshot.
// Generation runs at `v_root` with a relative `cmd/v`, as gen_vc_ci.yml always did: the checkout
// path is embedded in the snapshot, the output path is not.
fn generate_snapshot(mut log Log, v_root string, commit string, out_dir string, timeout_minutes int) ! {
	root := os.real_path(v_root)
	os.mkdir_all(out_dir)!
	out_root := os.real_path(out_dir)
	vexe := os.join_path(root, 'v')
	mut contents := map[string]string{}
	for name, flags in snapshot_files {
		out := os.join_path(out_root, name)
		os.rm(out) or {}
		mut args := [vexe, '-o', out]
		args << flags
		args << 'cmd/v'
		log.run_limited(in_dir(root, args), timeout_minutes, 'generating ${name} failed')!
		content := commit_hash_line(commit) + os.read_file(out)!
		os.write_file(out, content)!
		contents[name] = content
	}
	check_snapshot(commit, contents['v.c'], contents['v_win.c'])!
	log.note('${out_root}: v.c and v_win.c of ${commit} passed the snapshot checks')
}

// check_snapshot runs the sanity checks that gen_vc_ci.yml ran on every generated snapshot.
fn check_snapshot(commit string, v_c string, v_win_c string) ! {
	for name, content in {
		'v.c':     v_c
		'v_win.c': v_win_c
	} {
		if !content.starts_with(commit_hash_line(commit)) {
			return error('${name} does not start with `${commit_hash_line(commit).trim_space()}`')
		}
		if !content.contains('Turned ON custom defines: no_backtrace,cross')
			|| !content.contains('#define CUSTOM_DEFINE_cross') {
			return error('${name} was not generated with `-cross`')
		}
		// FastC's libtcc linking and Mach-O signing are host specific, so e.g. macOS `make` could
		// not compile a snapshot generated on Linux. The compiler built from the snapshot rebuilds
		// cmd/v natively, which keeps FastC.
		if content.contains('fastc__') {
			return error('${name} contains the FastC backend')
		}
		lines := content.count('\n')
		if lines <= snapshot_min_lines {
			return error('${name} has only ${lines} lines')
		}
	}
	// v.c also bootstraps 32-bit hosts: `int` is a typedef that the C compiler's target sizes, and
	// the system-header libc variant for 32-bit targets is merged in.
	if !v_c.contains('\ntypedef i32 __v_int;\n') {
		return error('v.c does not define `int` as the target sized `__v_int`')
	}
	if !v_c.contains('\n#define __V_HEADERLESS_LIBC 0\n') {
		return error('v.c does not merge in the system-header libc variant')
	}
}
