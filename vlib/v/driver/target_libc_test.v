module driver

import v.pref

fn libc_test_target(os_name string, arch string) pref.Target {
	return pref.Target{
		os:   os_name
		arch: arch
	}
}

fn test_v3_should_infer_host_libc_only_for_native_linked_builds() {
	host := libc_test_target('linux', 'amd64')
	assert v3_should_infer_host_libc(false, false, '', false, host, host)
	assert !v3_should_infer_host_libc(true, false, '', false, host, host)
	assert !v3_should_infer_host_libc(false, true, '', false, host, host)
	assert !v3_should_infer_host_libc(false, false, 'build/c-project', false, host, host)
	assert !v3_should_infer_host_libc(false, false, '', true, host, host)
	assert !v3_should_infer_host_libc(false, false, '', false,
		libc_test_target('windows', 'amd64'), host)
	assert !v3_should_infer_host_libc(false, false, '', false,
		libc_test_target('linux', 'arm64'), host)
}

fn test_v3_set_libc_define_replaces_the_other_libc() {
	mut defines := ['feature', 'glibc', 'glibc=forced']
	mut values := {
		'feature': 'true'
		'glibc':   'forced'
	}
	v3_set_libc_define(mut defines, mut values, 'musl')
	assert 'feature' in defines
	assert defines.any(it == 'musl')
	assert !defines.any(it.all_before('=').trim_space() == 'glibc')
	assert values['musl'] == 'true'
	assert 'glibc' !in values

	v3_set_libc_define(mut defines, mut values, 'glibc')
	assert defines.any(it == 'glibc')
	assert !defines.any(it.all_before('=').trim_space() == 'musl')
	assert values['glibc'] == 'true'
	assert 'musl' !in values
}

fn test_v3_has_libc_define_recognizes_plain_and_valued_defines() {
	assert !v3_has_libc_define(['feature'])
	assert v3_has_libc_define(['glibc'])
	assert v3_has_libc_define(['musl=true'])
}

fn test_v3_c_compiler_implies_musl_for_musl_gcc_wrappers() {
	assert v3_c_compiler_implies_musl('musl-gcc')
	assert v3_c_compiler_implies_musl('/usr/bin/x86_64-linux-musl-gcc')
	assert !v3_c_compiler_implies_musl('gcc')
	assert !v3_c_compiler_implies_musl('/usr/bin/clang')
}
