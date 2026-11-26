module driver

import os
import v.pref

// A dependency prefix contains target-built headers, archives and pkg-config
// metadata. It supplies native dependencies, never a second C runtime.
fn v3_freestanding_native_prefix(target pref.Target, defines []string, backend string, is_o bool, is_shared bool, modules string, configured string) !string {
	if backend != 'c' || target.os != 'linux' || !v3_omit_system_libc(defines) || 'vlibc_shim_object' in defines
		|| (('v_bare_no_startup' in defines || 'v_bare_libc' in defines) && !v3_needs_vlibc_shim(defines))
		|| is_o || is_shared {
		return ''
	}
	mut prefix := configured
	if prefix == '' {
		// A native installation must never become a cross compiler's sysroot.
		host := pref.host_target()
		if target.os != host.os || target.arch != host.arch { return '' }
		profile := if 'v_bare_static' in defines { 'static' } else { 'shared' }
		prefix = os.join_path(modules, 'vlibc', '${target.os}-${target.arch}-${profile}')
		if !os.is_file(os.join_path(prefix, 'vlibc-target')) { return '' }
	}
	prefix = os.real_path(prefix)
	marker := os.read_file(os.join_path(prefix, 'vlibc-target')) or {
		return error('vlibc dependency prefix `${prefix}` is not installed: missing vlibc-target')
	}
	expected := 'vlibc-native-v1\n${target.os}\n${target.arch}\n'
	if marker != expected {
		return error('vlibc dependency prefix `${prefix}` does not match ${target.os}/${target.arch}')
	}
	if !os.is_dir(os.join_path(prefix, 'lib')) || !os.is_dir(os.join_path(prefix, 'include')) {
		return error('vlibc dependency prefix `${prefix}` is incomplete: missing lib or include directory')
	}
	return prefix
}

fn v3_freestanding_pkgconfig_path(prefix string, existing string) string {
	mut paths := [os.join_path(prefix, 'lib', 'pkgconfig'), os.join_path(prefix, 'share', 'pkgconfig')]
	if existing != '' { paths << existing }
	return paths.join(os.path_delimiter)
}
