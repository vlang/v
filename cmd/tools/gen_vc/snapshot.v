module main

import os
import strings

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

struct SnapshotInclude {
	path  string
	start int
	end   int
}

fn snapshot_quoted_include(line string) ?string {
	directive := line[1..].trim_space()
	if !directive.starts_with('include') {
		return none
	}
	arg := directive['include'.len..].trim_space()
	if !arg.starts_with('"') {
		return none
	}
	end := arg.index_after('"', 1) or { return none }
	return arg[1..end]
}

fn next_snapshot_include(content string, offset int) ?SnapshotInclude {
	mut start := offset
	for start < content.len {
		end := content.index_after('\n', start) or { content.len }
		mut directive := start
		for directive < end && content[directive] in [` `, `\t`] {
			directive++
		}
		if directive < end && content[directive] == `#` {
			if path := snapshot_quoted_include(content[directive..end]) {
				return SnapshotInclude{
					path:  path
					start: start
					end:   if end < content.len { end + 1 } else { end }
				}
			}
		}
		start = end + 1
	}
	return none
}

fn snapshot_include_is_checkout_relative(path string) bool {
	return path.all_before('/') in ['vlib', 'thirdparty', 'cmd'] && path.contains('/')
		&& !path.split('/').any(it in ['', '.', '..'])
}

// relocate_snapshot_includes rewrites paths in generated C. The receiving C
// compiler opens the headers from a matching checkout and processes their includes.
fn relocate_snapshot_includes(root string, name string, content string) !string {
	root_prefix := root.replace('\\', '/').trim_right('/') + '/'
	mut relocated := strings.new_builder(0)
	mut copied := 0
	for {
		include := next_snapshot_include(content, copied) or { break }
		path := include.path.replace('\\', '/')
		relative := path.trim_string_left(root_prefix)
		if !snapshot_include_is_checkout_relative(relative) {
			return error('${name} has a non-portable include: ${include.path}')
		}
		relocated.write_string(content[copied..include.start])
		relocated.write_string(content[include.start..include.end].replace_once('"${include.path}"',
			'"${relative}"'))
		copied = include.end
	}
	if copied == 0 {
		return content
	}
	relocated.write_string(content[copied..])
	return relocated.str()
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
// Generation runs at `v_root` with a relative `cmd/v`, as gen_vc_ci.yml always did.
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
		relocated := relocate_snapshot_includes(root, name, os.read_file(out)!)!
		content := commit_hash_line(commit) + relocated
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
		mut offset := 0
		for {
			include := next_snapshot_include(content, offset) or { break }
			if !snapshot_include_is_checkout_relative(include.path) {
				return error('${name} has a non-portable include: ${include.path}')
			}
			offset = include.end
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
