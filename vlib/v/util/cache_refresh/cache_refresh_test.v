module cache_refresh

import os

fn test_synthetic_refresh_preserves_source_driver_flags() ! {
	$if !linux && !macos {
		return
	}
	os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'vup_source_driver_flags_${os.getpid()}')
	vroot := os.dir(@VEXE)
	name := 'vup_sanitizer_${os.getpid()}'
	module_root := os.join_path(vroot, 'vlib', name)
	os.mkdir_all(module_root)!
	os.mkdir_all(root)!
	saved := os.environ()
	defer {
		for variable in ['V3CACHE', 'VEXE', 'VFLAGS', 'CFLAGS', 'LDFLAGS', 'V3_CACHE_REFRESH',
			'V3_CACHE_REFRESH_ENTRY', 'V3_CACHE_REFRESH_PROJECT_FLAGS', 'V_SKIP_VVMRC'] {
			if variable in saved {
				os.setenv(variable, saved[variable], true)
			} else {
				os.unsetenv(variable)
			}
		}
		os.rmdir_all(module_root) or {}
		os.rmdir_all(root) or {}
	}
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	os.setenv('VEXE', @VEXE, true)
	os.setenv('V_SKIP_VVMRC', '1', true)
	for variable in ['VFLAGS', 'CFLAGS', 'LDFLAGS', 'V3_CACHE_REFRESH', 'V3_CACHE_REFRESH_ENTRY',
		'V3_CACHE_REFRESH_PROJECT_FLAGS'] {
		os.unsetenv(variable)
	}
	module_file := os.join_path(module_root, 'sample.v')
	os.write_file(module_file, 'module ${name}\npub fn value() int { return 1 }\n')!
	entry := os.join_path(root, 'main.v')
	os.write_file(entry, '#flag -fsanitize=address\nimport ${name}\nfn main() { println(${name}.value()) }\n')!
	args := ['-cc', 'clang', '-gc', 'none', '-o', os.join_path(root, 'program'), entry]
	cold := os.exec([@VEXE, ...args])
	assert cold.exit_code == 0, cold.output
	requests := collect(vroot, @VEXE)!.filter(it.module == name)
	assert requests.len == 1, requests.str()
	assert requests[0].project_flags.any('-fsanitize=address' in it.flags), requests.str()
	os.write_file(module_file, 'module ${name}\npub fn value() int { return 2 }\n')!
	// Exercise the synthetic fallback too: its instrumented module objects
	// must stay compatible with the original program's compiler flags.
	refresh(@VEXE, requests.map(Request{ ...it, entry: '' }))!
	warm := os.exec([@VEXE, ...args])
	assert warm.exit_code == 0, warm.output
	assert !warm.output.contains('Caching module '), warm.output
}

fn test_inventory_preserves_compiler_modes_and_flags_and_excludes_project_modules() ! {
	root := os.join_path(os.vtmp_dir(), 'vup_inventory_${os.getpid()}')
	os.mkdir_all(root)!
	previous := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', root, true)
	defer {
		if value := previous { os.setenv('V3CACHE', value, true) } else { os.unsetenv('V3CACHE') }
		os.rmdir_all(root) or {}
	}
	vroot := os.join_path(root, 'checkout')
	source := os.join_path(vroot, 'vlib', 'crypto', 'sample', 'sample.v')
	builtin := os.join_path(vroot, 'vlib', 'builtin', 'builtin.v')
	local := os.join_path(root, 'project', 'local.v')
	for file in [source, builtin, local] {
		os.mkdir_all(os.dir(file))!
		os.write_file(file, 'module sample\n')!
	}
	sources := {
		'sample':  [source]
		'builtin': [builtin]
		'local':   [local]
	}
	mut expected := []Request{}
	for compiler in ['tcc', 'cc', 'clang', 'gcc'] {
		for prod in [false, true] {
			mut flags := ['-cc', compiler, '-cflags', '-DSAMPLE="value with spaces"']
			if prod { flags << '-prod' }
			dir := os.join_path(root_dir(vroot, @VEXE), '${compiler}_${prod}')
			os.mkdir_all(dir)!
			project_flags := [FlagGroup{ flags: ['-DSAMPLE=value with spaces'], in_main: true }]
			record(dir, vroot, flags, false, sources, project_flags, '', false)!
			// Recording the same build twice must not duplicate or lose modules.
			record(dir, vroot, flags, false, sources, project_flags, '', false)!
			for name in ['builtin', 'crypto.sample'] {
				expected << Request{
					args:          flags
					module:        name
					project_flags: project_flags
					imports:       ['crypto.sample']
				}
			}
		}
	}
	requests := collect(vroot, @VEXE)!
	assert requests.len == expected.len
	for request in expected {
		assert request in requests, request.str()
	}
	assert !requests.any(it.module == 'local')
}

fn test_inventory_skips_unreadable_truncated_and_future_records() ! {
	root := os.join_path(os.vtmp_dir(), 'vup_invalid_records_${os.getpid()}')
	vroot := os.join_path(root, 'checkout')
	source := os.join_path(vroot, 'vlib', 'sample', 'sample.v')
	previous := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	defer {
		if value := previous { os.setenv('V3CACHE', value, true) } else { os.unsetenv('V3CACHE') }
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.dir(source))!
	os.write_file(source, 'module sample\n')!
	dir := os.join_path(root_dir(vroot, @VEXE), 'config')
	os.mkdir_all(dir)!
	record(dir, vroot, ['-cc', 'cc'], false, {
		'sample': [source]
	}, [], '', false)!
	// A directory reliably makes read_file fail on every supported platform.
	os.mkdir(os.join_path(dir, '${record_prefix}unreadable'))!
	os.write_file(os.join_path(dir, '${record_prefix}truncated'), '${record_format}\nsample\nprogram\n100\nshort')!
	os.write_file(os.join_path(dir, '${record_prefix}future'), 'vup-cache-refresh-99\nsample\nprogram\n')!
	assert collect(vroot, @VEXE)! == [Request{
		args:    ['-cc', 'cc']
		module:  'sample'
		imports: ['sample']
	}]
}

fn test_live_inventory_retires_changed_sources_and_preserves_current_variants() ! {
	root := os.join_path(os.vtmp_dir(), 'vup_entry_identity_${os.getpid()}')
	vroot := os.join_path(root, 'checkout')
	source := os.join_path(vroot, 'vlib', 'sample', 'sample.v')
	entry := os.join_path(root, 'project', 'main.v')
	local := os.join_path(root, 'old modules', 'local', 'local.v')
	new_local := os.join_path(root, 'new modules', 'new_local', 'local.v')
	listed := os.join_path(root, 'listed')
	previous := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	defer {
		if value := previous { os.setenv('V3CACHE', value, true) } else { os.unsetenv('V3CACHE') }
		os.rmdir_all(root) or {}
	}
	for path in [source, entry, local] { os.mkdir_all(os.dir(path))! }
	os.mkdir_all(listed)!
	os.write_file(source, 'module sample\n')!
	os.write_file(entry, 'module main\nimport local\nfn main() { local.run() }\n')!
	os.write_file(local, 'module local\npub fn run() {}\n')!
	modules := {
		'sample': [source]
		'local':  [local]
	}
	old_dir := os.join_path(root_dir(vroot, @VEXE), 'old-config')
	os.mkdir_all(old_dir)!
	record(old_dir, vroot, ['-path', os.dir(os.dir(local)), '-file-list', listed], false,
		modules, [], entry, false)!
	// The stale inventory still owns this header and must not generate a legacy matrix.
	os.write_file(os.join_path(old_dir, 'sample_${hash_text(os.real_path(os.dir(source)))}.vh'), '')!
	old := collect(vroot, @VEXE)!
	assert old.len == 1 && old[0].entry_sources.len == 3, old.str()
	os.write_file(entry, 'module main\nimport new_local\nfn main() { new_local.run() }\n')!
	assert collect(vroot, @VEXE)!.len == 0
	// Saved snapshots must be checked again before their child is started.
	refresh(os.join_path(vroot, 'missing-v'), old)!
	os.mkdir_all(os.dir(new_local))!
	os.write_file(new_local, 'module new_local\npub fn run() {}\n')!
	current_modules := {
		'sample':    [source]
		'new_local': [new_local]
	}
	for compiler in ['cc', 'clang', 'tcc'] {
		for prod in [false, true] {
			mut args := ['-cc', compiler, '-path', os.join_path(root, 'new modules'), '-file-list',
				listed]
			if prod { args << '-prod' }
			dir := os.join_path(root_dir(vroot, @VEXE), '${compiler}_${prod}')
			os.mkdir_all(dir)!
			record(dir, vroot, args, false, current_modules, [], entry, false)!
		}
	}
	current := collect(vroot, @VEXE)!
	assert current.len == 6 && current.all(it.args[0] == '-cc'), current.str()
	// Changing an imported project source can invalidate the configuration even
	// when the entry file itself is unchanged.
	os.write_file(new_local, 'module new_local\nimport new_dependency\npub fn run() {}\n')!
	assert collect(vroot, @VEXE)!.len == 0
	refresh(os.join_path(vroot, 'missing-v'), current)!
	os.write_file(new_local, 'module new_local\npub fn run() {}\n')!
	assert collect(vroot, @VEXE)!.len == 6
	// A new file in the same imported module can require a new search path too.
	added_import := os.join_path(os.dir(new_local), 'added.v')
	os.write_file(added_import, 'module new_local\nimport new_dependency\n')!
	assert collect(vroot, @VEXE)!.len == 0
	refresh(os.join_path(vroot, 'missing-v'), current)!
	os.rm(added_import)!
	assert collect(vroot, @VEXE)!.len == 6
	os.write_file(os.join_path(listed, 'added.v'), 'module main\nstruct Added {}\n')!
	assert collect(vroot, @VEXE)!.len == 0
}

fn test_inventory_handles_legacy_headers_and_partly_recorded_configurations() ! {
	root := os.join_path(os.vtmp_dir(), 'vup_legacy_inventory_${os.getpid()}')
	os.mkdir_all(root)!
	previous := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', root, true)
	defer {
		if value := previous { os.setenv('V3CACHE', value, true) } else { os.unsetenv('V3CACHE') }
		os.rmdir_all(root) or {}
	}
	vroot := os.join_path(root, 'checkout')
	source := os.join_path(vroot, 'vlib', 'crypto', 'sample', 'sample.v')
	os.mkdir_all(os.dir(source))!
	os.write_file(source, 'module sample\n')!
	for config in ['old', 'new'] {
		dir := os.join_path(root_dir(vroot, @VEXE), config)
		os.mkdir_all(dir)!
		os.write_file(os.join_path(dir, 'sample_${hash_text(os.real_path(os.dir(source)))}.vh'), '')!
		// A module recorded in another configuration must not hide this old one.
		if config == 'new' {
			record(dir, vroot, ['-cc', 'custom-cc'], false, {
				'sample': [source]
			}, [], '', false)!
		}
	}
	requests := collect(vroot, @VEXE)!
	assert Request{ args: ['-cc', 'custom-cc'], module: 'crypto.sample', imports: ['crypto.sample'] } in requests
	legacy := requests.filter(it.legacy_headers.len > 0)
	assert legacy.all(it.legacy_headers.len == 1)
	plain := legacy.map(Request{ ...it, legacy_headers: map[string]string{} })
	if _ := os.find_abs_path_of_executable('cc') {
		for is_test in [false, true] {
			assert Request{ args: ['-cc', 'cc'], module: 'crypto.sample', is_test: is_test } in plain
			assert Request{ args: ['-cc', 'cc', '-prod'], module: 'crypto.sample', is_test: is_test } in plain
		}
	}
	assert requests.all(it.module == 'crypto.sample')
}

fn test_legacy_migration_retires_only_successfully_refreshed_headers() ! {
	$if windows {
		return
	}
	os.find_abs_path_of_executable('cc') or { return }
	root := os.join_path(os.vtmp_dir(), 'vup_legacy_migration_${os.getpid()}')
	vroot := os.join_path(root, 'checkout')
	vexe := os.join_path(vroot, 'v')
	previous := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', root, true)
	defer {
		if value := previous { os.setenv('V3CACHE', value, true) } else { os.unsetenv('V3CACHE') }
		os.rmdir_all(root) or {}
	}
	mut headers := []string{}
	mut sources := map[string][]string{}
	for name in ['sample', 'other'] {
		source := os.join_path(vroot, 'vlib', name, '${name}.v')
		os.mkdir_all(os.dir(source))!
		os.write_file(source, 'module ${name}\n')!
		sources[name] = [source]
		for config in ['old-compiler-a', 'old-compiler-b'] {
			dir := os.join_path(root_dir(vroot, vexe), config)
			os.mkdir_all(dir)!
			header := os.join_path(dir, '${name}_${hash_text(os.real_path(os.dir(source)))}.vh')
			os.write_file(header, 'legacy ${name}\n')!
			if name == 'sample' { headers << header }
		}
	}
	failure := os.join_path(root, 'fail')
	os.write_file(failure, '')!
	os.write_file(vexe, '#!/bin/sh\nif test -f ' + os.quoted_path(failure) + '; then exit 1; fi\nexit 0\n')!
	os.chmod(vexe, 0o755)!
	requests := collect(vroot, vexe)!.filter(it.module == 'sample')
	assert requests.len > 0
	assert requests.all(it.legacy_headers.len == 2), requests.str()
	if _ := refresh(vexe, requests) {
		assert false, 'failed migrations must remain retryable'
	}
	assert headers.all(!os.exists('${it}.vup_migrated'))
	assert collect(vroot, vexe)!.filter(it.module == 'sample') == requests
	os.rm(failure)!
	// Rebuilding V creates records in another configuration, leaving the old
	// headers behind. Those headers must stop generating fallback requests.
	new_dir := os.join_path(root_dir(vroot, vexe), 'updated-compiler')
	os.mkdir_all(new_dir)!
	record(new_dir, vroot, ['-cc', 'cc'], false, {
		'sample': sources['sample']
	}, [], '', false)!
	refresh(vexe, requests)!
	assert headers.all(os.exists('${it}.vup_migrated'))
	after := collect(vroot, vexe)!
	assert after.filter(it.module == 'sample') == [Request{
		args:    ['-cc', 'cc']
		module:  'sample'
		imports: ['sample']
	}]
	assert after.any(it.module == 'other' && it.legacy_headers.len == 2)
	// A subsequently rewritten legacy header needs a new successful migration.
	os.write_file(headers[0], 'different legacy contents\n')!
	changed := collect(vroot, vexe)!.filter(it.module == 'sample' && it.legacy_headers.len > 0)
	assert changed.len == requests.len
	assert changed.all(it.legacy_headers.len == 1)
	refresh(vexe, changed)!
	assert !collect(vroot, vexe)!.any(it.module == 'sample' && it.legacy_headers.len > 0)
}

fn test_legacy_migration_does_not_retire_skipped_variants() ! {
	root := os.join_path(os.vtmp_dir(), 'vup_skipped_migration_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'vlib', 'sample'))!
	defer { os.rmdir_all(root) or {} }
	header := os.join_path(root, 'sample.vh')
	os.write_file(header, 'legacy\n')!
	refresh(os.join_path(root, 'v'), [Request{
		args:           ['-cc', os.join_path(root, 'missing-cc')]
		module:         'sample'
		legacy_headers: {
			header: hash_text('legacy\n')
		}
	}])!
	assert !os.exists('${header}.vup_migrated')
}

fn test_inventory_shares_cwd_independent_requests_between_projects() ! {
	root := os.join_path(os.vtmp_dir(), 'vup_shared_inventory_${os.getpid()}')
	os.mkdir_all(root)!
	previous := os.environ()
	previous_dir := os.getwd()
	defer {
		os.chdir(previous_dir) or {}
		if 'V3CACHE' in previous {
			os.setenv('V3CACHE', previous['V3CACHE'], true)
		} else {
			os.unsetenv('V3CACHE')
		}
		os.rmdir_all(root) or {}
	}
	os.setenv('V3CACHE', root, true)
	vroot := os.join_path(root, 'checkout')
	source := os.join_path(vroot, 'vlib', 'sample', 'sample.v')
	os.mkdir_all(os.dir(source))!
	os.write_file(source, 'module sample\n')!
	dir := os.join_path(root_dir(vroot, @VEXE), 'config')
	os.mkdir_all(dir)!
	for project in ['one', 'two'] {
		cwd := os.join_path(root, project)
		os.mkdir_all(cwd)!
		os.chdir(cwd)!
		record(dir, vroot, ['-cc', 'cc'], false, {
			'sample': [source]
		}, [], '', false)!
	}
	requests := collect(vroot, @VEXE)!
	assert requests.len == 1 && requests[0].cwd == '', requests.str()
	assert !flags_need_cwd(['-cflags', '-DPROJECT -include-pch /absolute/header.pch -I/absolute/include'], [])
	assert flags_need_cwd(['-cflags', '-I include'], [])
	assert flags_need_cwd([], [FlagGroup{ flags: ['-include', 'config.h'] }])
}

fn test_module_names_do_not_allow_source_injection() {
	assert valid_module('crypto.sha3')
	assert valid_module('builtin.closure')
	for invalid in ['', '.', 'a..b', '../local', 'sample\nfn main() {}', 'sample as _'] {
		assert !valid_module(invalid), invalid
	}
}

fn test_inventory_argument_encoding_preserves_newlines_and_empty_values() ! {
	request := Request{
		args:            ['-cc', 'cc', '-cflags', '-DNAME="value with spaces"\n-DSECOND', '', '-prod']
		module:          'crypto.sha3'
		is_test:         true
		cwd:             os.join_path(os.getwd(), 'directory with spaces\nand newlines')
		project_flags:   [FlagGroup{
			flags:    ['-DNAME=value with spaces\nand newlines', '-I', 'include path', '']
			at_start: true
			in_main:  true
			identity: 'group\x00with\x00newlines\n'
		}]
		imports:         ['crypto.sha3', 'encoding.hex']
		entry:           os.join_path(os.getwd(), 'project with spaces', 'main.v')
		show_test_stats: true
		compile_inputs:  ['/path with spaces/header.h']
		entry_sources:   {
			os.abs_path('project with spaces/main.v'): 'file:1234abcd'
		}
	}
	assert decode_request(encode_request(request))! == request
	assert !encode_project_flags(request.project_flags).contains('\x00')
	assert decode_request('${project_record_format}\n${request.module}\ntest\n' +
		encode_arguments([request.cwd, encode_project_flags(request.project_flags),
			encode_arguments(request.imports), ...request.args]))! == Request{ ...request, entry: '', show_test_stats: false, compile_inputs: []string{}, entry_sources: map[string]string{} }
	assert decode_request('${entry_record_format}\n${request.module}\ntest\n' +
		encode_arguments([request.cwd, encode_project_flags(request.project_flags),
			encode_arguments(request.imports), request.entry, ...request.args]))! == Request{ ...request, show_test_stats: false, compile_inputs: []string{}, entry_sources: map[string]string{} }
	assert decode_request('${input_record_format}\n${request.module}\ntest\n' +
		encode_arguments([request.cwd, encode_project_flags(request.project_flags),
			encode_arguments(request.imports), request.entry, request.show_test_stats.str(),
			encode_arguments(request.compile_inputs), ...request.args]))! == Request{ ...request, entry_sources: map[string]string{} }
	assert !entry_sources_unchanged(Request{ ...request, entry_sources: map[string]string{} })
	assert decode_request('${object_record_format}\ncrypto.sha3\nprogram\n0\n4\ntrue1\n17\n-DSTALE3\n-cc2\ncc')! == Request{
		args:   ['-cc', 'cc']
		module: 'crypto.sha3'
	}
	assert decode_request('${cwd_record_format}\ncrypto.sha3\nprogram\n0\n3\n-cc2\ncc')! == Request{
		args:   ['-cc', 'cc']
		module: 'crypto.sha3'
	}
	assert decode_request('${legacy_record_format}\ncrypto.sha3\nprogram\n3\n-cc2\ncc')! == Request{
		args:   [
			'-cc',
			'cc',
		]
		module: 'crypto.sha3'
	}
	for content in ['', 'old-format\ncrypto.sha3\nprogram\n',
		'${record_format}\ncrypto.sha3\nprogram\n100\nshort'] {
		if _ := decode_request(content) {
			assert false, content
		}
	}
}

fn test_refresh_skips_removed_modules_without_starting_the_compiler() ! {
	root := os.join_path(os.vtmp_dir(), 'vup_removed_module_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'vlib'))!
	defer { os.rmdir_all(root) or {} }
	// There is no compiler here: attempting to compile the retired import fails.
	refresh(os.join_path(root, 'v'), [Request{ args: ['-cc', 'cc'], module: 'retired' }])!
}

fn test_refresh_skips_vanished_and_non_executable_c_compilers() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vup_vanished_compiler_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'vlib', 'sample'))!
	defer { os.rmdir_all(root) or {} }
	missing := os.join_path(root, 'missing-cc')
	non_executable := os.join_path(root, 'non-executable-cc')
	live := os.join_path(root, 'live-cc')
	for path in [non_executable, live] {
		os.write_file(path, '#!/bin/sh\nexit 0\n')!
		os.chmod(path, if path == live { 0o755 } else { 0o600 })!
	}
	log := os.join_path(root, 'calls')
	vexe := os.join_path(root, 'v')
	os.write_file(vexe, '#!/bin/sh\nprintf "%s\\n" "\$2" >> ' + os.quoted_path(log) + '\n')!
	os.chmod(vexe, 0o755)!
	refresh(vexe, [missing, non_executable, live].map(Request{ args: ['-cc', it], module: 'sample' }))!
	assert os.read_file(log)!.split_into_lines() == [live]
}

fn test_refresh_uses_live_entries_once_and_never_runs_or_links_them() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vup_live_entry_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'vlib', 'sample'))!
	os.mkdir_all(os.join_path(root, 'vlib', 'other'))!
	defer { os.rmdir_all(root) or {} }
	entry := os.join_path(root, 'main.v')
	os.write_file(entry, 'fn main() { panic("must never run") }\n')!
	log := os.join_path(root, 'calls')
	vexe := os.join_path(root, 'v')
	os.write_file(vexe, '#!/bin/sh\n' +
		'test "\$V3_CACHE_REFRESH_ENTRY" = 1 || exit 2\n' +
		'test -z "\$V3_CACHE_REFRESH_PROJECT_FLAGS" || exit 3\n' +
		'printf "%s\\n" "\$*" >> ' + os.quoted_path(log) + '\n')!
	os.chmod(vexe, 0o755)!
	mut requests := ['sample', 'other'].map(Request{
		module:        it
		entry:         entry
		entry_sources: {
			entry: entry_source_signature(entry)!
		}
		project_flags: [FlagGroup{ flags: ['-DOLD'], in_main: true }]
	})
	requests << Request{
		...requests[0]
		project_flags:  [FlagGroup{ flags: ['-DNEW'], in_main: true }]
		imports:        ['other']
		compile_inputs: [entry]
		is_test:        true // Also unused: the live entry and arguments select test mode.
	}
	refresh(vexe, requests)!
	lines := os.read_file(log)!.split_into_lines()
	assert lines.len == 1, lines.str()
	assert lines[0].contains('-skip-running'), lines[0]
	assert lines[0].ends_with(entry), lines[0]
	// Distinct child arguments, working directories, entries and environment
	// settings must still execute separately.
	other_entry := os.join_path(root, 'other.v')
	os.write_file(other_entry, 'fn main() {}\n')!
	refresh(vexe, [
		Request{ ...requests[0], args: ['-prod'] },
		Request{ ...requests[0], cwd: root },
		Request{
			...requests[0]
			entry:         other_entry
			entry_sources: {
				other_entry: entry_source_signature(other_entry)!
			}
		},
		Request{ ...requests[0], show_test_stats: true },
	])!
	assert os.read_file(log)!.split_into_lines().len == 5
}

fn test_inventory_and_refresh_skip_vanished_working_directories() ! {
	root_path := os.join_path(os.vtmp_dir(), 'vup_vanished_directory_${os.getpid()}')
	os.mkdir_all(root_path)!
	root := os.real_path(root_path)
	source := os.join_path(root, 'vlib', 'sample', 'sample.v')
	project := os.join_path(root, 'project')
	previous_cache := os.getenv_opt('V3CACHE')
	previous_dir := os.getwd()
	os.mkdir_all(os.dir(source))!
	os.mkdir_all(project)!
	os.write_file(source, 'module sample\n')!
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	defer {
		os.chdir(previous_dir) or {}
		if value := previous_cache {
			os.setenv('V3CACHE', value, true)
		} else {
			os.unsetenv('V3CACHE')
		}
		os.rmdir_all(root) or {}
	}
	dir := os.join_path(root_dir(root, @VEXE), 'config')
	os.mkdir_all(dir)!
	// A stale record still owns its header; it must not fall back to legacy refreshes.
	os.write_file(os.join_path(dir, 'sample_${hash_text(os.real_path(os.dir(source)))}.vh'), '')!
	os.chdir(project)!
	record(dir, root, ['-cflags', '-include config.h -I include'], false, {
		'sample': [source]
	}, [], '', false)!
	os.chdir(previous_dir)!
	requests := collect(root, @VEXE)!
	assert requests.len == 1 && requests[0].cwd == project
	os.mv(project, '${project}_moved')!
	assert collect(root, @VEXE)!.len == 0
	// There is no compiler: a failed child chdir or attempted compile would fail.
	refresh(os.join_path(root, 'v'), requests)!
	os.rmdir_all('${project}_moved')!
	refresh(os.join_path(root, 'v'), requests)!
	// Live records in the same configuration remain refreshable.
	record(dir, root, ['-cc', 'cc'], false, {
		'sample': [source]
	}, [], '', false)!
	assert collect(root, @VEXE)! == [Request{
		args:    ['-cc', 'cc']
		module:  'sample'
		imports: ['sample']
	}]
}

fn test_refresh_uses_recorded_directory_for_relative_compile_inputs() ! {
	$if windows {
		return
	}
	root := os.join_path(os.real_path(os.vtmp_dir()), 'vup_relative_inputs_${os.getpid()}')
	project := os.join_path(root, 'project with spaces')
	os.mkdir_all(os.join_path(project, 'include path'))!
	os.write_file(os.join_path(project, 'config.h'), '#include <refresh_input.h>\n')!
	os.write_file(os.join_path(project, 'include path', 'refresh_input.h'), '#define REFRESH_INPUT 1\n')!
	source := os.join_path(root, 'vlib', 'crypto', 'sample', 'sample.v')
	os.mkdir_all(os.dir(source))!
	os.write_file(source, 'module sample\n')!
	previous_dir := os.getwd()
	defer {
		os.chdir(previous_dir) or {}
		os.rmdir_all(root) or {}
	}
	dir := os.join_path(root, 'config')
	os.mkdir_all(dir)!
	os.chdir(project)!
	record(dir, root, ['-cflags', '-include config.h -I "include path"'], false, {
		'sample': [source]
	}, [FlagGroup{ flags: ['-include', 'config.h', '-I', 'include path'], in_main: true }], '', false)!
	entries := os.ls(dir)!.filter(it.starts_with(record_prefix))
	assert entries.len == 1
	request := decode_request(os.read_file(os.join_path(dir, entries[0]))!)!
	assert request.cwd == project
	assert request.compile_inputs == [os.join_path(project, 'config.h')]
	compiler := os.join_path(root, 'v')
	log := os.join_path(root, 'cwd.log')
	os.write_file(compiler, '#!/bin/sh\n' +
		'test "\$1" = "-cflags" || exit 2\n' +
		'test "\$2" = \'-include config.h -I "include path"\' || exit 3\n' +
		'test -f config.h && test -f "include path/refresh_input.h" || exit 4\n' +
		'test -z "\$V3_CACHE_REFRESH_OBJECT_FLAGS" || exit 5\n' +
		'printf "%s" "\$V3_CACHE_REFRESH_PROJECT_FLAGS" > ' + os.quoted_path('${log}.flags') + '\n' +
		'printf "%s" "\$PWD" > ' + os.quoted_path(log) + '\n')!
	os.chmod(compiler, 0o755)!
	os.chdir(root)!
	refresh(compiler, [request])!
	assert os.read_file(log)! == project
	assert decode_project_flags(os.read_file('${log}.flags')!)! == request.project_flags
	assert os.getwd() == root
}

fn test_forced_headers_bypass_refresh_inventory_for_each_compiler_and_mode() ! {
	$if windows {
		return
	}
	vroot := os.dir(@VEXE)
	root := os.join_path(os.real_path(os.vtmp_dir()), 'vup_cache_integration_${os.getpid()}')
	os.mkdir_all(root)!
	project := os.join_path(root, 'project with spaces')
	os.mkdir_all(os.join_path(project, 'modules'))!
	library_dir := os.join_path(root, 'libs')
	os.mkdir_all(library_dir)!
	c_source := os.join_path(library_dir, 'answer.c')
	c_object := os.join_path(library_dir, 'answer.o')
	os.write_file(c_source, 'int answer(void) { return 42; }\n')!
	for command in [['cc', '-c', c_source, '-o', c_object],
		['ar', 'rcs', os.join_path(library_dir, 'libanswer.a'), c_object]] {
		result := os.exec(command)
		assert result.exit_code == 0, result.output
	}
	module_root := os.join_path(vroot, 'vlib', 'v', 'util', 'cache_refresh', 'testdata',
		'refresh_${os.getpid()}')
	import_root := 'v.util.cache_refresh.testdata.refresh_${os.getpid()}'
	dependency := os.join_path(module_root, 'dependency', 'dependency.v')
	probe := os.join_path(module_root, 'probe', 'probe.v')
	for file in [dependency, probe] { os.mkdir_all(os.dir(file))! }
	// Native search paths inside vlib support module caching. A relative link
	// reaches this directory from the project, but not from the updater's cwd.
	os.mkdir_all(os.join_path(module_root, 'include'))!
	old_module_header := os.join_path(module_root, 'include', 'old_module.h')
	new_module_header := os.join_path(module_root, 'include', 'new_module.h')
	os.write_file(old_module_header, '#define REFRESH_MODULE_INPUT 1\n')!
	os.symlink(os.join_path(module_root, 'include'), os.join_path(root, 'include path'))!
	os.symlink(os.join_path(module_root, 'include'), os.join_path(root, 'include'))!
	os.write_file(dependency, 'module dependency\n#flag -DREFRESH_DEPENDENCY=old\n' +
		'#flag -DREFRESH_REMOVED\n#flag -include "${old_module_header}"\n' +
		'pub interface Reader { read() int }\n' +
		'pub fn value() int { return 40 }\n')!
	os.write_file(probe, 'module probe\nimport ${import_root}.dependency\n' +
		'pub fn read(reader dependency.Reader) int { return reader.read() }\n' +
		'pub fn value() int {\n\t\$if prod { return dependency.value() + 2 }\n' +
		'\treturn dependency.value()\n}\n')!
	saved := os.environ()
	os.unsetenv('V3CACHE')
	os.setenv('VTMP', os.join_path(root, 'cache'), true)
	os.setenv('V3_TEST_ISOLATE_CACHE', '1', true)
	os.setenv('VEXE', @VEXE, true)
	os.setenv('V_SKIP_VVMRC', '1', true)
	os.unsetenv('VFLAGS')
	os.unsetenv('CFLAGS')
	os.unsetenv('LDFLAGS')
	os.unsetenv('V3_CACHE_FORCE_SOURCE')
	os.unsetenv('V3_CACHE_TRACE')
	os.unsetenv('V3_CACHE_REFRESH')
	os.unsetenv('VTEST_ONLY_FN')
	os.unsetenv('VTEST_SHOW_ASSERTS')
	previous_dir := os.getwd()
	defer {
		for name in ['V3CACHE', 'VTMP', 'V3_TEST_ISOLATE_CACHE', 'VEXE', 'V_SKIP_VVMRC', 'VFLAGS',
			'CFLAGS', 'LDFLAGS', 'PATH', 'V3_CACHE_FORCE_SOURCE', 'V3_CACHE_TRACE', 'V3_CACHE_REFRESH',
			'VTEST_ONLY_FN', 'VTEST_SHOW_ASSERTS'] {
			if name in saved { os.setenv(name, saved[name], true) } else { os.unsetenv(name) }
		}
		os.chdir(previous_dir) or {}
		os.rmdir_all(module_root) or {}
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(project, 'main.v')
	support_file := os.join_path(root, 'shared support.v')
	sink_file := os.join_path(root, 'implementers', 'nested', 'sink.v')
	os.mkdir_all(os.dir(sink_file))!
	os.write_file(support_file, 'module main\nimport ${import_root}.probe\n' +
		'fn listed_value() int { return probe.value() }\n')!
	os.write_file(sink_file, 'module main\nstruct Sink {}\n' +
		'fn (Sink) read() int { return listed_value() }\n')!
	forced_header := os.join_path(module_root, 'include', 'config.h')
	os.write_file(forced_header, '#define REFRESH_SOURCE_INPUT 1\n')!
	source_flags := '#flag -DREFRESH_PROJECT\n#flag -include "${forced_header}"\n'
	os.write_file(main_file, source_flags + 'import ${import_root}.probe\n' +
		'fn main() { println(probe.read(Sink{})) }\n')!
	mut supported_modes := ['-no-std', '-strict']
	$if linux {
		libc := os.exec(['ldd', '--version'])
		supported_modes << if libc.output.contains('musl') { '-musl' } else { '-glibc' }
	}
	os.chdir(project)!
	mut builds := [][]string{}
	for compiler in ['cc', 'clang', 'gcc', 'tcc'] {
		if compiler == 'tcc' {
			if !os.is_executable(os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')) { continue }
		} else {
			os.find_abs_path_of_executable(compiler) or { continue }
		}
		mut modes := [][]string{len: 1}
		if compiler == 'cc' {
			for mode in supported_modes { modes << [mode] }
		}
		for mode in modes {
			for prod in [false, true] {
				// TinyCC is not a -prod compiler, so there is no production TinyCC build to check.
				if compiler == 'tcc' && prod { continue }
				mut flags := ['-cc', compiler, '-gc', 'none', '-path', 'modules/../modules|@vlib',
					'-file-list', '../shared support.v,../implementers']
				if prod { flags << '-prod' }
				flags << mode
				if compiler == 'cc' && !prod { flags << ['-ldflags', '-L../libs -lanswer'] }
				if compiler == 'cc' {
					flags << ['-cflags', '-L../libs -lanswer -DREFRESH_LABEL="a b"']
				}
				flags << ['-cflags', '-I "../include path"']
				if compiler == 'gcc' && prod { flags << '-silent' }
				mut process := os.new_process(@VEXE)
				process.set_args([...flags, '-o', os.join_path(root, 'program'), 'run', main_file])
				process.set_redirect_stdio()
				process.run()
				stdout := process.stdout_slurp()
				stderr := process.stderr_slurp()
				process.wait()
				assert process.code == 0, '${flags}: ${stdout}\n${stderr}'
				process.close()
				assert stdout.trim_space() == if prod { '42' } else { '40' }, '${flags}: ${stdout}\n${stderr}'
				assert !stderr.contains('Caching module '), stderr
				builds << flags
			}
		}
	}
	// Cover both direct selectors and vtest's transient VTEST_ONLY_FN transport.
	test_file := os.join_path(project, 'selected_test.v')
	os.write_file(test_file, source_flags + 'import ${import_root}.probe\n' +
		'fn test_selected_one() { assert probe.value() > 0 }\n' +
		'fn test_selected_two() { assert probe.value() > 0 }\n' +
		'fn test_unselected() { panic("unselected test ran") }\n')!
	mut test_builds := [][]string{}
	mut assert_modes := []bool{}
	for show_asserts in [false, true] {
		if show_asserts {
			os.setenv('VTEST_SHOW_ASSERTS', '1', true)
		} else {
			os.unsetenv('VTEST_SHOW_ASSERTS')
		}
		for through_vtest in [false, true] {
			if through_vtest {
				// vtest replaces VTMP with a disposable session directory. Pin its
				// cache to the existing isolated compiler cache instead.
				os.setenv('V3CACHE', os.dir(root_dir(vroot, @VEXE)), true)
			}
			selector := if through_vtest {
				'test_selected_one, test_selected_two'
			} else {
				'test_selected_*'
			}
			for prod in [false, true] {
				mut flags := ['-cc', 'cc', '-gc', 'none', '-path', 'modules/../modules|@vlib',
					'-cflags', '-I../include']
				if prod { flags << '-prod' }
				command := if through_vtest {
					[@VEXE, ...flags, 'test', '-run-only', selector, test_file]
				} else {
					[@VEXE, ...flags, '-run-only', selector, '-o', os.join_path(root, 'test_program'),
						test_file]
				}
				result := os.exec(command)
				assert result.exit_code == 0, result.output
				flags << ['-run-only', selector]
				test_builds << flags
				assert_modes << show_asserts
			}
			os.unsetenv('V3CACHE')
		}
	}
	os.unsetenv('VTEST_SHOW_ASSERTS')
	// Keep the initial records, then extend the listed directory and make the
	// current entry require its new declaration. A source flag edit also leaves
	// historical records whose obsolete source identities must be retired.
	initial := collect(vroot, @VEXE)!.filter(it.module.starts_with(import_root + '.') && !it.is_test)
	old_flags := builds[0].clone()
	new_search := os.join_path(root, 'updated modules')
	new_module := 'current_support_${os.getpid()}'
	new_module_source := os.join_path(new_search, new_module, 'support.v')
	os.mkdir_all(os.dir(new_module_source))!
	os.write_file(new_module_source, 'module ${new_module}\npub fn extra() int { return 0 }\n')!
	added_file := os.join_path(root, 'implementers', 'nested', 'added.v')
	os.write_file(added_file, 'module main\nfn newly_listed_reader() Sink { return Sink{} }\n')!
	os.write_file(main_file, '#flag -DREFRESH_CHANGED_PROJECT\n' +
		os.read_file(main_file)!.replace('import ${import_root}.probe', 'import ${import_root}.probe\nimport ${new_module}')
			.replace('probe.read(Sink{})', 'probe.read(newly_listed_reader()) + ${new_module}.extra()'))!
	// The historical configuration cannot compile the current entry: its search
	// path cannot find the new project import. It must not be replayed by v up.
	old_build := os.exec([@VEXE, ...old_flags, '-check', main_file])
	assert old_build.exit_code != 0 && old_build.output.contains(new_module), old_build.output
	for index in 0 .. builds.len {
		mut flags := builds[index].clone()
		flags[flags.index('-path') + 1] = '${new_search}|@vlib'
		builds[index] = flags
		result := os.exec([@VEXE, ...flags, '-o', os.join_path(root, 'program'), main_file])
		assert result.exit_code == 0, result.output
	}
	requests := collect(vroot, @VEXE)!.filter(it.module.starts_with(import_root + '.'))
	// Forced headers have no native ownership contract. Every configuration
	// builds correctly, but none may publish a refreshable module inventory.
	assert initial.len == 0 && requests.len == 0, requests.str()
	os.write_file(new_module_header, '#define REFRESH_MODULE_INPUT 2\n')!
	os.rm(old_module_header)!
	os.write_file(dependency, 'module dependency\n#flag -DREFRESH_DEPENDENCY=new\n' +
		'#flag -DREFRESH_ADDED\n#flag -include "${new_module_header}"\n' +
		'pub interface Reader { read() int }\n' +
		'pub fn value() int { return 43 }\n')!
	for flags in builds {
		result := os.exec([@VEXE, ...flags, '-o', os.join_path(root, 'program'), main_file])
		assert result.exit_code == 0, result.output
		run := os.exec([os.join_path(root, 'program')])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == if '-prod' in flags { '45' } else { '43' }, run.output
	}
	for index, flags in test_builds {
		if assert_modes[index] {
			os.setenv('VTEST_SHOW_ASSERTS', '1', true)
		} else {
			os.unsetenv('VTEST_SHOW_ASSERTS')
		}
		result := os.exec([@VEXE, ...flags, '-o', os.join_path(root, 'test_program'), test_file])
		assert result.exit_code == 0, result.output
	}
	assert collect(vroot, @VEXE)!.all(!it.module.starts_with(import_root + '.'))
}

fn test_inventory_and_refresh_skip_replaced_compile_inputs() ! {
	$if windows {
		return
	}
	root_path := os.join_path(os.vtmp_dir(), 'vup_replaced_inputs_${os.getpid()}')
	os.mkdir_all(root_path)!
	root := os.real_path(root_path)
	project := os.join_path(root, 'project')
	include_dir := os.join_path(project, 'include path')
	source := os.join_path(root, 'vlib', 'sample', 'sample.v')
	previous_dir := os.getwd()
	previous_cache := os.getenv_opt('V3CACHE')
	os.mkdir_all(include_dir)!
	os.mkdir_all(os.dir(source))!
	os.write_file(source, 'module sample\n')!
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	defer {
		os.chdir(previous_dir) or {}
		if value := previous_cache {
			os.setenv('V3CACHE', value, true)
		} else {
			os.unsetenv('V3CACHE')
		}
		os.rmdir_all(root) or {}
	}
	dir := os.join_path(root_dir(root, @VEXE), 'config')
	os.mkdir_all(dir)!
	os.chdir(project)!
	mut recorded := []Request{}
	for name in ['old.h', 'new.h'] {
		path := os.join_path(include_dir, name)
		os.write_file(path, '#define VALUE 1\n')!
		args := ['-cflags', '-I "include path" -include ${name}']
		project_flags := [FlagGroup{ flags: ['-imacros', path], in_main: true }]
		record(dir, root, args, false, {
			'sample': [source]
		}, project_flags, '', false)!
		request := collect(root, @VEXE)!.filter(it.args == args)[0]
		assert request.compile_inputs == [path], request.str()
		recorded << request
	}
	os.rm(os.join_path(include_dir, 'old.h'))!
	assert os.is_dir(project)
	assert collect(root, @VEXE)! == [recorded[1]]
	log := os.join_path(root, 'calls')
	vexe := os.join_path(root, 'v')
	os.write_file(vexe, '#!/bin/sh\nprintf "%s\\n" "\$*" >> ' + os.quoted_path(log) + '\n')!
	os.chmod(vexe, 0o755)!
	refresh(vexe, recorded)!
	lines := os.read_file(log)!.split_into_lines()
	assert lines.len == 1 && lines[0].contains('new.h') && !lines[0].contains('old.h'), lines.str()
}

fn test_refresh_replays_and_clears_test_assert_display_modes() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vup_assert_modes_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'vlib', 'sample'))!
	previous := os.getenv_opt('VTEST_SHOW_ASSERTS')
	os.setenv('VTEST_SHOW_ASSERTS', 'ambient', true)
	defer {
		if value := previous {
			os.setenv('VTEST_SHOW_ASSERTS', value, true)
		} else {
			os.unsetenv('VTEST_SHOW_ASSERTS')
		}
		os.rmdir_all(root) or {}
	}
	log := os.join_path(root, 'calls')
	vexe := os.join_path(root, 'v')
	os.write_file(vexe, '#!/bin/sh\nprintf "%s\\n" "\${VTEST_SHOW_ASSERTS-unset}" >> ' + os.quoted_path(log) + '\n')!
	os.chmod(vexe, 0o755)!
	refresh(vexe, [true, false].map(Request{ module: 'sample', is_test: true, show_test_stats: it }))!
	assert os.read_file(log)!.split_into_lines() == ['1', 'unset']
	assert os.getenv('VTEST_SHOW_ASSERTS') == 'ambient'
}

fn test_refresh_skips_production_tinycc_alias_before_starting_builds() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'cache_refresh_prod_tcc_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	alias := os.join_path(root, 'cc')
	os.write_file(alias, '#!/bin/sh\nprintf "tcc version 0.9.27\\n"\n')!
	os.chmod(alias, 0o700)!
	request := Request{ args: ['-prod', '-cc', './cc'], cwd: root, module: 'builtin' }
	assert request_uses_prod_tcc(request)
	equals_request := Request{ ...request, args: ['-prod', '-cc=./cc'] }
	assert request_uses_prod_tcc(equals_request)
	refresh(os.join_path(root, 'missing-v'), [request])!
	project := os.join_path(root, 'tinycc-project')
	os.mkdir_all(project)!
	os.mkdir_all(os.join_path(root, 'vlib', 'builtin'))!
	wrapper := os.join_path(root, 'context-wrapper')
	os.write_file(wrapper, '#!/bin/sh\ncase "$PWD" in\n*/tinycc-project) echo "tcc version 0.9.27" ;;\n*) echo "clang version 19.1.7" ;;\nesac\n')!
	os.chmod(wrapper, 0o700)!
	marker := os.join_path(root, 'refresh-started')
	fake_v := os.join_path(root, 'fake-v')
	os.write_file(fake_v, '#!/bin/sh\necho started > ' + os.quoted_path(marker) + '\nexit 91\n')!
	os.chmod(fake_v, 0o700)!
	before := os.getwd()
	for args in [['-prod', '-cc', wrapper], ['-prod', '-cc', '../context-wrapper'],
		['-prod', '-cc=../context-wrapper']] {
		context_request := Request{ ...request, args: args, cwd: project }
		assert request_uses_prod_tcc(context_request)
		refresh(fake_v, [context_request])!
		assert !os.exists(marker)
	}
	assert !request_uses_prod_tcc(Request{ ...request, args: ['-prod', '-cc', wrapper] })
	assert os.getwd() == before
	bare := 'cache_context_cc_${os.getpid()}'
	os.cp(wrapper, os.join_path(project, bare))!
	os.chmod(os.join_path(project, bare), 0o700)!
	saved_path := os.getenv('PATH')
	defer { os.setenv('PATH', saved_path, true) }
	os.setenv('PATH', '.' + os.path_delimiter + saved_path, true)
	context_request := Request{ ...request, args: ['-prod', '-cc', bare], cwd: project }
	assert request_uses_prod_tcc(context_request)
	refresh(fake_v, [context_request])!
	assert !os.exists(marker)
	assert os.getenv('PATH') == '.' + os.path_delimiter + saved_path
	assert os.getwd() == before
}
