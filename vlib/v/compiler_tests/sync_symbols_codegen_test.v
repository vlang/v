module main

import os
import v.cmdexec

fn test_sync_symbols_keep_v_functions_separate_from_posix_calls() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'sync_symbols_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	c_source := os.join_path(root, 'linux.c')
	os.mkdir_all(os.join_path(root, 'probe'))!
	os.write_file(os.join_path(root, 'probe', 'probe.c.v'), r'module probe
$if linux {
 #include <unistd.h>
 fn C.syncfs(fd int) int
}

// invalid_fd_result reports the failed syncfs request for an invalid descriptor.
pub fn invalid_fd_result() int {
 $if linux {
  return C.syncfs(-1)
 } $else {
  return -1
 }
}
')!
	fixture := r'module main
import probe
#include <unistd.h>

struct Settings {
 value int
}

fn sync(settings Settings) int { return settings.value + 1 }
fn syncfs(fd int) int { return fd + 2 }
fn v_sync(settings Settings) int { return settings.value + 3 }
fn v_syncfs(fd int) int { return fd + 4 }
fn main__sync(settings Settings) int { return settings.value + 5 }
fn main__syncfs(fd int) int { return fd + 6 }
fn __v3_internal_symbol_libc_sync(settings Settings) int { return settings.value + 7 }
fn __v3_internal_symbol_libc_syncfs(fd int) int { return fd + 8 }

fn invoke_sync(callback fn (Settings) int, settings Settings) int {
 return callback(settings)
}

fn invoke_syncfs(callback fn (int) int, fd int) int {
 return callback(fd)
}

fn main() {
 settings := Settings{value: 40}
 assert sync(settings) == 41
 assert syncfs(40) == 42
 assert v_sync(settings) == 43
 assert v_syncfs(40) == 44
 assert main__sync(settings) == 45
 assert main__syncfs(40) == 46
 assert __v3_internal_symbol_libc_sync(settings) == 47
 assert __v3_internal_symbol_libc_syncfs(40) == 48
 assert invoke_sync(sync, settings) == 41
 assert invoke_syncfs(syncfs, 40) == 42
 assert invoke_sync(v_sync, settings) == 43
 assert invoke_syncfs(v_syncfs, 40) == 44
 assert invoke_sync(main__sync, settings) == 45
 assert invoke_syncfs(main__syncfs, 40) == 46
 assert invoke_sync(__v3_internal_symbol_libc_sync, settings) == 47
 assert invoke_syncfs(__v3_internal_symbol_libc_syncfs, 40) == 48
 assert probe.invalid_fd_result() == -1
 println("sync symbols OK")
}
'
	os.write_file(source, fixture)!
	generated := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-gc', 'none', '-os', 'linux',
		'-o', c_source, source])
	assert generated.exit_code == 0, generated.output
	code := os.read_file(c_source)!
	feature_macro := code.index('#define _GNU_SOURCE') or { -1 }
	first_include := code.index('#include') or { -1 }
	assert feature_macro >= 0 && first_include > feature_macro
	assert code.contains('main__sync(')
	assert code.contains('main__syncfs(')
	assert code.contains(' __v3_internal_symbol_libc_sync(')
	assert code.contains(' __v3_internal_symbol_libc_syncfs(')
	assert code.contains(' __v3_internal_symbol_source___v3_internal_symbol_libc_sync(')
	assert code.contains(' __v3_internal_symbol_source___v3_internal_symbol_libc_syncfs(')
	assert code.contains('v_sync(')
	assert code.contains('v_syncfs(')
	assert code.contains('return syncfs(-1);')
	// A private cache covers both its initial population and reuse, without
	// relying on another test's cached modules.
	for flags in [[]string{}, []string{}, ['-nocache']] {
		mut args := ['V3CACHE=${os.join_path(root, 'cache')}', @VEXE, '-new-compiler', '-gc', 'none']
		args << flags
		args << ['run', source]
		ran := cmdexec.run('env', args)
		assert ran.exit_code == 0, ran.output
		assert ran.output.trim_space() == 'sync symbols OK', ran.output
	}
	// Declaring C.sync already participates in the existing collision lookup.
	// Check its function reference separately so it cannot mask the regression.
	explicit_c_sync := fixture.replace('fn sync(settings', 'fn C.sync()\n\nfn sync(settings')
		.replace(' println("sync symbols OK")', ' native_sync := C.sync\n assert !isnil(native_sync)\n println("sync symbols OK")')
	os.write_file(source, explicit_c_sync)!
	explicit_generated := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-gc', 'none', '-os',
		'linux', '-o', c_source, source])
	assert explicit_generated.exit_code == 0, explicit_generated.output
	explicit_code := os.read_file(c_source)!
	assert explicit_code.contains('= sync;')
	assert explicit_code.contains('main__sync(')
	assert explicit_code.contains(' __v3_internal_symbol_libc_sync(')
}
