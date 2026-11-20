module driver

import os
import v.pref

fn test_mbedtls_vendored_include_path_matches_only_the_vendored_tree() {
	sep := os.path_separator
	vroot := ['C:', 'repo'].join(sep)
	vendored_include := os.join_path(vroot, 'thirdparty', 'mbedtls', 'include')
	assert mbedtls_vendored_include_path(vendored_include, vroot)
	assert mbedtls_vendored_include_path('C:/repo/thirdparty/mbedtls/library', 'C:/repo')
	assert mbedtls_vendored_include_path(os.join_path(vroot, 'thirdparty', 'mbedtls'),
		vroot)
	// Joined flag forms still match the vendored tree.
	assert mbedtls_vendored_include_path('-I' + vendored_include, vroot)
	assert mbedtls_vendored_include_path('-isystem' + vendored_include, vroot)
	// An app-owned tree with the same suffix is user code and must be kept.
	app_tree := os.join_path('project', 'thirdparty', 'mbedtls', 'include')
	assert !mbedtls_vendored_include_path(app_tree, vroot)
	assert !mbedtls_vendored_include_path('-I' + app_tree, vroot)
	assert !mbedtls_vendored_include_path(os.join_path('project', 'sysinclude', 'mbedtls'),
		vroot)
	assert !mbedtls_vendored_include_path('/usr/include', vroot)
	assert !mbedtls_vendored_include_path('', vroot)
	assert !mbedtls_vendored_include_path(vendored_include, '')
}

fn test_without_mbedtls_vendored_include_dirs_keeps_user_flags() {
	sep := os.path_separator
	vroot := ['C:', 'repo'].join(sep)
	vendored := os.join_path(vroot, 'thirdparty', 'mbedtls', 'include')
	user_dir := os.join_path('project', 'sysinclude')
	app_tree := os.join_path('project', 'thirdparty', 'mbedtls', 'include')
	flags := ['-I', vendored, '-I', user_dir, '-I' + vendored, '-D', 'X=1', '-I', app_tree,
		'-isystem' + app_tree, user_dir + '.c']
	filtered := without_mbedtls_vendored_include_dirs(flags, vroot)
	assert filtered == ['-I', user_dir, '-D', 'X=1', '-I', app_tree, '-isystem' + app_tree,
		user_dir + '.c'], filtered.str()
	assert without_mbedtls_vendored_include_dirs([]string{}, vroot) == []
}

// A link plan written by the pre-#32422 policy (v5, bare C sources passed
// through under the full flags) must not be reused after the upgrade to
// user-plan objects (v6): the cached plan path changed and the old format
// no longer validates, so a warmed cache is recomputed instead of linking
// stale objects.
fn test_link_plan_rejects_pre_user_plan_format() {
	root := os.join_path(os.vtmp_dir(), 'v link plan upgrade ${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	object := os.join_path(root, 'real.o')
	os.write_file(object, 'object')!
	stale := os.join_path(root, 'stale.manifest')
	os.write_file(stale, 'format=v3-c-link-plan-v5\nrequests=1\ndirect_objects=1\ndependency_files=0\nflag=${object}\nobject=${object}\ncomplete=1\n')!
	mut stats := CObjectCacheStats{}
	assert valid_c_link_plan(stale, mut stats) == none
}

// The native compilation policy (which search dirs user C sees) is part of
// the link plan identity: the same flags with and without the vendored
// Mbed TLS dirs must resolve to different plan files.
fn test_link_plan_path_covers_user_flag_plan() {
	target := pref.host_target()
	compiler := 'v_mbedtls_plan_probe_compiler'
	mut stats := CObjectCacheStats{
		compiler_versions: {
			compiler: 'fixture compiler identity'
		}
	}
	flags := ['-I', os.join_path(@VEXEROOT, 'thirdparty', 'mbedtls', 'include'), 'user.c']
	full := CObjectFlagPlan{
		primary_compiler: compiler
		common_flags:     c_object_compile_support_flags(flags)
	}
	user := CObjectFlagPlan{
		primary_compiler: compiler
		common_flags:     without_mbedtls_vendored_include_dirs(c_object_compile_support_flags(flags),
			@VEXEROOT)
	}
	assert full.common_flags != user.common_flags
	first := c_link_plan_path(root_for_link_plan(), flags, &full, &full, false, false,
		'', [], target, compiler, false, mut stats)
	second := c_link_plan_path(root_for_link_plan(), flags, &full, &user, false, false,
		'', [], target, compiler, false, mut stats)
	assert first != second, '${first} vs ${second}'
}

fn root_for_link_plan() string {
	return os.join_path(os.vtmp_dir(), 'v3_thirdparty_objs')
}
