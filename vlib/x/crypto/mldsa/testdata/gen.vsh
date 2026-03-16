#!/usr/bin/env -S v run

import os

// generates ML-DSA test vectors by building Go's reference implementation
// and running gen.go with crypto/internal/fips140/mldsa
//
// Usage:
//   v run testdata/gen.vsh
//   v run testdata/gen.vsh <go-source-path> # uses existing Go tree

const go_repo = 'https://github.com/golang/go.git'
const go_ref = 'master'
const go_mldsa_subpath = os.join_path('src', 'crypto', 'internal', 'fips140', 'mldsa')
const go_gen_pkg = os.join_path('.', go_mldsa_subpath, 'gen')

fn main() {
	unbuffer_stdout()
	script_dir := os.dir(@FILE)

	mut go_root := os.join_path(os.temp_dir(), 'go')
	if os.args.len > 1 {
		go_root = os.args[1]
		if !os.exists(os.join_path(go_root, go_mldsa_subpath)) {
			eprintln('error: ${go_root} does not look like a Go source tree')
			exit(1)
		}
		println('> Using existing Go tree at ${go_root}')
	} else {
		if os.exists(os.join_path(go_root, 'src')) {
			println('> Reusing cached Go tree at ${go_root}')
			run_or_exit('git -C ${go_root} pull --quiet')
		} else {
			println('> Cloning Go source tree to ${go_root}...')
			run_or_exit('git clone --depth 1 --branch ${go_ref} ${go_repo} ${go_root}')
		}
	}

	go_bin := os.join_path(go_root, 'bin', 'go')
	if !os.exists(go_bin) {
		println('> Building Go toolchain...')
		run_or_exit('cd ${go_root}/src && bash make.bash')
	}
	println('> Go binary: ${go_bin}')

	gen_dst_dir := os.join_path(go_root, go_mldsa_subpath, 'gen')
	os.mkdir_all(gen_dst_dir) or {
		eprintln('error: mkdir failed: ${err}')
		exit(1)
	}
	os.cp(os.join_path(script_dir, 'gen.go'), os.join_path(gen_dst_dir, 'main.go')) or {
		eprintln('error: copy failed: ${err}')
		exit(1)
	}

	println('> Generating test vectors...')
	res := run_or_exit('cd ${go_root} && GOROOT=${go_root} ${go_bin} run ${go_gen_pkg}')

	os.rmdir_all(gen_dst_dir) or { eprintln('warning: rmdir failed: ${err}') }

	vectors_path := os.join_path(script_dir, 'vectors.json')
	os.write_file(vectors_path, res) or {
		eprintln('error: write failed: ${err}')
		exit(1)
	}
	println('> Wrote ${vectors_path}')
}

fn run_or_exit(cmd string) string {
	res := os.execute_opt(cmd) or {
		eprintln('error: ${err}')
		exit(1)
	}
	if res.exit_code != 0 {
		eprintln('error: ${cmd}\n${res.output}')
		exit(1)
	}
	return res.output
}
