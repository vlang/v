module c

fn test_system_libc_thread_preamble_uses_native_windows_api() {
	mut g := FlatGen.new()
	g.system_libc_preamble()
	c_code := g.sb.str()
	windows_start := c_code.index('#ifdef _WIN32') or { panic('missing Windows guard') }
	posix_start := c_code.index('#else\ntypedef struct { pthread_t handle; } __v_thread;') or {
		panic('missing POSIX fallback')
	}
	windows_code := c_code[windows_start..posix_start]
	assert windows_code.contains('CreateThread('), windows_code
	assert windows_code.contains('WaitForSingleObject('), windows_code
	assert windows_code.contains('CloseHandle('), windows_code
	assert !windows_code.contains('pthread_'), windows_code
}

fn test_late_source_context_does_not_replay_multiline_header_body() {
	directives := [
		'#define SOKOL_IMPL\n#ifndef SOKOL_INCLUDED\ntypedef int sokol_value;\n#endif',
		'#if defined(SOKOL_IMPL)\ntypedef int sokol_impl_value;\n#endif',
		'#include "/tmp/native.m"',
	]
	emission := c_source_directive_emission(directives, map[string]bool{})
	assert 0 !in emission.emit_late
	assert 1 !in emission.emit_late
	assert emission.emit_late[2]
	assert emission.skip_early[2]
}
