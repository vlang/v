module driver

import os
import crypto.sha256
import v.cmdexec
import v.tempname
import v.pref

fn v3_omit_system_libc(defines []string) bool {
	return 'no_system_libc' in defines || ('freestanding' in defines && 'v_bare_libc' !in defines)
}

fn v3_needs_vlibc_shim(defines []string) bool {
	return 'no_system_libc' in defines && (('vlibc' in defines && 'freestanding' !in defines)
		|| ('syscalls' in defines && 'v_bare_libc' in defines))
}

// Only V's replacement startup initializes TLS through the GNU loader.
fn v3_uses_bare_dynamic_loader(defines []string) bool {
	return ('freestanding' in defines || 'vlibc' in defines) && v3_omit_system_libc(defines)
		&& 'v_bare_static' !in defines && ('v_bare_no_startup' !in defines || v3_needs_vlibc_shim(defines))
}

fn shim_tool(program string, args []string) !string {
	result := cmdexec.run(program, args)
	if result.exit_code != 0 {
		return error('${program} failed (${result.exit_code}): ${result.output}')
	}
	return result.output.trim_space()
}

fn shim_globals(nm string, object string) ![]string {
	list := shim_tool(nm, ['-g', '--defined-only', '--format=posix', object])!
	return list.split_into_lines().filter(it != '').map(it.all_before(' '))
}

// Compile the replacement as a separate translation unit. Only the C ABI may
// cross into the application, whose V builtins and collector have their own state.
fn v3_build_vlibc_shim(dir string, cc string, target pref.Target, target_args []string, static_link bool) !string {
	if target.os != 'linux' || target.arch !in ['amd64', 'arm64'] {
		return error('standalone vlibc requires a Linux amd64 or arm64 target')
	}
	work := os.abs_path(os.join_path(dir, 'vlibc-shim'))
	os.mkdir_all(work)!
	entry := os.join_path(work, 'entry.v')
	source := os.join_path(work, 'runtime.c')
	flags_path := os.join_path(work, 'flags')
	os.write_file(entry, 'module main\n')!
	mut args := ['-new-compiler', '-no-retry-compilation', '-os', target.os, '-arch', target.arch,
		'-cc', cc, '-freestanding', '-gc', 'none', '-d', 'no_main', '-d', 'vlibc_shim_object']
	if !static_link { args << ['-cflags', '-DV_BARE_DYNAMIC_LOADER=1'] }
	args << ['-dump-c-flags', flags_path, '-o', source, entry]
	mut env := os.environ()
	env['VFLAGS'] = ''
	res := cmdexec.run_in_merged_with_env(os.executable(), args, work, env)
	if res.exit_code != 0 { return error(res.output) }
	flags := os.read_file(flags_path)!.split('\x00')
	mut compile := c_object_compile_flags(flags)
	compile << ['-O2', '-g0', '-fno-builtin']
	mut stats := CObjectCacheStats{}
	mut objects := []string{}
	// Give identical emitted C a stable source path for the native dependency
	// manifest. Includes still undergo the usual toolchain dependency scan.
	content := os.read_file(source)!
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_thirdparty_objs')
	ensure_native_object_cache_dir(cache_dir)!
	cached_source := os.join_path(cache_dir, 'vlibc_' + sha256.hexhash(content) + '.c')
	if !os.is_file(cached_source) {
		stage := cached_source + '.tmp.' + tempname.unique_token()
		os.write_file(stage, content)!
		publish_native_c_object(stage, cached_source)!
	}
	generated := compile_c_object_with_manifest(cached_source + '.o', cached_source, cc,
		[...target_args, ...compile], target, work, mut stats)!
	for index in c_link_input_indices(flags) {
		path := flags[index]
		if path.ends_with('.c') {
			objects << compile_c_object_with_manifest(path + '.o', path, cc,
				[...target_args, ...compile], target, work, mut stats)!
		} else if path.ends_with('.o') || path.ends_with('.a') {
			objects << path
		}
	}
	combined := os.join_path(work, 'combined.o')
	shim_tool(cc, [...target_args, '-nostdlib', '-no-pie', '-r', generated, ...objects, '-o', combined])!
	nm := shim_tool(cc, [...target_args, '-print-prog-name=nm'])!
	objcopy := shim_tool(cc, [...target_args, '-print-prog-name=objcopy'])!
	mut public := {
		'_start': true
	}
	for name in os.read_lines(source + '.exports')! { public[name] = true }
	mut private := map[string]bool{}
	for name in shim_globals(nm, generated)! {
		if !public[name] { private[name] = true }
	}
	for name in shim_globals(nm, combined)! {
		if name in ['v_freestanding_environment_lock', 'v_freestanding_environment_unlock',
			'v_freestanding_environment_get_locked'] {
			continue
		}
		if name.starts_with('v_') || name.starts_with('__v_') { private[name] = true }
	}
	list := os.join_path(work, 'private-symbols')
	os.write_lines(list, private.keys())!
	output := os.join_path(work, 'libvlibc.o')
	shim_tool(objcopy, ['--localize-symbols=' + list, combined, output])!
	return output
}
