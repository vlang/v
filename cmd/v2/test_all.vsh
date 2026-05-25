#!/usr/bin/env -S v

import os

const total_steps = 15
const v2_bak = '/tmp/v2_src_bak_test_all'
const cleanc_cache = '/tmp/v2_cleanc_obj_cache'
const gitly_clone_dir = '/tmp/v2_test_gitly_clone'
const gitly_repo_url = 'https://github.com/vlang/gitly'

struct Config {
	v1_compiler string
	script_dir  string
	repo_root   string
	v2_src      string
	arm_flag    string
}

fn main() {
	cfg := parse_config()
	os.chdir(cfg.script_dir) or { fail('failed to enter ${cfg.script_dir}: ${err}') }

	rm_rf(cleanc_cache)

	section(1, 'ARM64 self-host hello world')
	backup_v2_src(cfg)
	run('${q(cfg.v1_compiler)} -gc none -cc cc -o v2 v2.v')
	restore_v2_src(cfg)
	run('./v2 --no-parallel -backend arm64 -gc none -nocache ${cfg.arm_flag} -o v3 v2.v')
	run('./v3 --no-parallel -backend arm64 ${cfg.arm_flag} -o hello_arm hello.v')
	run('./hello_arm')
	cleanup_files(['v3', 'hello_arm'])

	section(2, 'ARM64 self-host chain (v2->v3->v4->v5->v6)')
	println('  Building v3 from v2...')
	run('./v2 --no-parallel -nocache -gc none -backend arm64 ${cfg.arm_flag} -o v3_chain v2.v')
	println('  Building v4 from v3...')
	run('./v3_chain --no-parallel -nocache -gc none -backend arm64 ${cfg.arm_flag} -o v4_chain v2.v')
	println('  Building v5 from v4...')
	run('./v4_chain --no-parallel -nocache -gc none -backend arm64 ${cfg.arm_flag} -o v5_chain v2.v')
	println('  Building v6 from v5...')
	run('./v5_chain --no-parallel -nocache -gc none -backend arm64 ${cfg.arm_flag} -o v6_chain v2.v')
	v5_size := os.file_size('v5_chain')
	v6_size := os.file_size('v6_chain')
	if v5_size == v6_size {
		println('  v5=v6 (${v5_size} bytes) - chain converged')
	} else {
		fail('  FAIL: v5 (${v5_size}) != v6 (${v6_size})')
	}
	cleanup_files(['v3_chain', 'v4_chain', 'v5_chain', 'v6_chain'])

	section(3, 'Self-host test')
	run('bash test_v2_self.sh')

	section(4, 'Builtin test files (cleanc)')
	rm_rf(cleanc_cache)
	run('./v2 ../../vlib/builtin/array_test.v')
	run('./v2 ../../vlib/builtin/string_test.v')
	run('./v2 ../../vlib/builtin/map_test.v')

	section(5, 'Builtin test files (arm64)')
	for arm64_test in ['array_test.v', 'string_test.v', 'map_test.v'] {
		run('./v2 -backend arm64 ${cfg.arm_flag} "../../vlib/builtin/${arm64_test}"')
	}

	section(6, 'Math test')
	run('./v2 ../../vlib/math/math_test.v')

	section(7, 'Math test (arm64)')
	run('./v2 -backend arm64 ${cfg.arm_flag} ../../vlib/math/math_test.v')

	section(8, 'Sumtype tests')
	for st in sumtype_tests() {
		run('./v2 ${st}')
	}

	section(9, 'Sumtype tests (arm64)')
	for st in sumtype_tests() {
		run('./v2 -backend arm64 ${cfg.arm_flag} ${st}')
	}

	section(10, 'SSA backends test (arm64)')
	run('${q(cfg.v1_compiler)} -gc none run test_ssa_backends.v arm64 ${cfg.arm_flag}')

	section(11, 'SSA backends test (cleanc)')
	run('${q(cfg.v1_compiler)} -gc none run test_ssa_backends.v cleanc')

	section(12, 'Transformer unit tests')
	run('${q(cfg.v1_compiler)} ../../vlib/v2/transformer/transformer_test.v')

	section(13, 'Transformer integration test')
	run('${q(cfg.v1_compiler)} ../../vlib/v2/transformer/transformer_v2_darwin_test.v')

	section(14, 'Cleanc runtime tests')
	run('${q(cfg.v1_compiler)} -gc none run ../../vlib/v2/gen/cleanc/tests/run_tests.v')

	section(15, 'Gitly build (cleanc, -d use_openssl -d sqlite)')
	v2_bin := os.join_path(cfg.script_dir, 'v2')
	gitly_dir := resolve_gitly_dir()
	os.chdir(gitly_dir) or { fail('failed to enter ${gitly_dir}: ${err}') }
	run('${q(v2_bin)} -v -showcc -o x -stats -d use_openssl -d sqlite .')
	cleanup_files(['x'])
	os.chdir(cfg.script_dir) or { fail('failed to return to ${cfg.script_dir}: ${err}') }

	println('')
	println('=== ALL TESTS PASSED ===')
}

fn resolve_gitly_dir() string {
	local := os.join_path(os.home_dir(), 'code', 'gitly')
	if os.is_dir(local) {
		println('  Using local gitly at ${local}')
		return local
	}
	if os.is_dir(gitly_clone_dir) {
		println('  Reusing cloned gitly at ${gitly_clone_dir}')
		return gitly_clone_dir
	}
	println('  Cloning ${gitly_repo_url} into ${gitly_clone_dir}...')
	run('git clone --depth 1 ${gitly_repo_url} ${q(gitly_clone_dir)}')
	return gitly_clone_dir
}

fn parse_config() Config {
	arm_flag := parse_args()
	script_dir := os.real_path(@DIR)
	repo_root := os.real_path(os.join_path(script_dir, '..', '..'))
	v1_compiler := absolute_path(os.getenv_opt('V') or { os.join_path(repo_root, 'v') })
	if !os.is_executable(v1_compiler) {
		fail('FAIL: v1 compiler not found: ${v1_compiler}')
	}
	return Config{
		v1_compiler: v1_compiler
		script_dir:  script_dir
		repo_root:   repo_root
		v2_src:      os.join_path(repo_root, 'vlib', 'v2')
		arm_flag:    arm_flag
	}
}

fn parse_args() string {
	mut arm_flag := '-O0'
	for arg in os.args[1..] {
		match arg {
			'-prod' {
				arm_flag = '-prod'
			}
			'-O0' {
				arm_flag = '-O0'
			}
			'-h', '--help' {
				println('Usage: v run cmd/v2/test_all.vsh [-prod|-O0]')
				println('')
				println('By default, ARM64 backend builds use -O0 and skip SSA optimization.')
				println('With -prod, ARM64 backend builds use -prod.')
				println('With -O0, ARM64 backend builds use the default no-optimization mode.')
				exit(0)
			}
			else {
				fail('unknown argument: ${arg}')
			}
		}
	}
	return arm_flag
}

fn absolute_path(path string) string {
	if os.is_abs_path(path) {
		return path
	}
	return os.join_path(os.getwd(), path)
}

fn section(step int, title string) {
	if step > 1 {
		println('')
	}
	println('=== ${step}/${total_steps}: ${title} ===')
}

fn backup_v2_src(cfg Config) {
	rm_rf(v2_bak)
	run('cp -R ${q(cfg.v2_src)} ${q(v2_bak)}')
}

fn restore_v2_src(cfg Config) {
	run('rsync -a --delete ${q(v2_bak + '/')} ${q(cfg.v2_src + '/')}')
}

fn rm_rf(path string) {
	if !os.exists(path) {
		return
	}
	if os.is_dir(path) {
		os.rmdir_all(path) or { fail('failed to remove ${path}: ${err}') }
		return
	}
	os.rm(path) or { fail('failed to remove ${path}: ${err}') }
}

fn cleanup_files(paths []string) {
	for path in paths {
		os.rm(path) or {}
	}
}

fn run(cmd string) {
	code := os.system(cmd)
	if code != 0 {
		exit(code)
	}
}

fn q(path string) string {
	return os.quoted_path(path)
}

fn fail(message string) {
	eprintln(message)
	exit(1)
}

fn sumtype_tests() []string {
	return [
		'test_sumtype.v',
		'test_sumtype2.v',
		'test_sumtype3.v',
		'test_sumtype4.v',
		'test_sumtype_pos.v',
		'test_sumtype_data.v',
		'test_sumtype_ifexpr.v',
		'test_sumtype_nested.v',
		'test_sumtype_many.v',
		'test_sumtype_global.v',
	]
}
