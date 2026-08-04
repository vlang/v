module builder

import os

struct TccMacosLibgcTestSource {
	lib_dir string
	target  string
	source  string
}

struct TccMacosLibgcTestFixture {
	root      string
	data_base string
	libgc     TccMacosLibgcTestSource
}

struct TccMacosLibgcTestContentStore {
	resolved_source string
	expected        TccMacosLibgcFileHash
	store_root      string
	content_dir     string
	final_dylib     string
}

fn tcc_macos_libgc_test_tokens(vroot string) (string, string) {
	lib_dir := os.join_path(vroot, 'thirdparty', 'tcc', 'lib')
	return '"${os.join_path(lib_dir, tcc_macos_libgc_name)}"', '-Wl,-rpath,"${lib_dir}"'
}

fn expect_tcc_macos_libgc_plan_error(vroot string, linker_flags []string, pre_args []string, expected_fragment string) {
	plan_tcc_macos_libgc_store(vroot, linker_flags, pre_args) or {
		assert err.msg().contains(expected_fragment), err.msg()
		return
	}
	assert false, 'expected macOS bundled libgc planning to fail'
}

fn create_tcc_macos_libgc_test_source(root string, name string, content string) !TccMacosLibgcTestSource {
	lib_dir := os.join_path(root, '${name} V root, with space', 'thirdparty', 'tcc', 'lib')
	os.mkdir_all(lib_dir, mode: 0o700)!
	target := os.join_path(lib_dir, 'libgc.1.dylib')
	source := os.join_path(lib_dir, tcc_macos_libgc_name)
	os.write_file(target, content)!
	os.chmod(target, 0o700)!
	os.symlink(os.file_name(target), source)!
	return TccMacosLibgcTestSource{
		lib_dir: lib_dir
		target:  target
		source:  source
	}
}

fn create_tcc_macos_libgc_test_fixture(name string, content string) !TccMacosLibgcTestFixture {
	root := os.join_path(os.vtmp_dir(), 'v_builder_tcc_macos_libgc_${os.getpid()}_${name}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root, mode: 0o700)!
	data_base := os.join_path(root, 'persistent data root')
	os.mkdir(data_base, mode: 0o700)!
	libgc := create_tcc_macos_libgc_test_source(root, 'primary', content)!
	return TccMacosLibgcTestFixture{
		root:      root
		data_base: data_base
		libgc:     libgc
	}
}

fn expect_tcc_macos_libgc_materialize_error(source string, data_base string, expected_fragment string) {
	materialize_tcc_macos_libgc_at(source, data_base) or {
		assert err.msg().contains(expected_fragment), err.msg()
		return
	}
	assert false, 'expected macOS bundled libgc materialization to fail'
}

fn expect_tcc_macos_libgc_data_base_error(xdg_data_home string, home string, rejected string, expected_reason string) {
	resolve_tcc_macos_libgc_data_base(xdg_data_home, home) or {
		message := err.msg()
		assert message.contains(rejected), message
		assert message.contains(expected_reason), message
		assert message.contains(tcc_macos_libgc_data_root_remediation), message
		return
	}
	assert false, 'expected macOS bundled libgc data root validation to fail'
}

fn assert_tcc_macos_libgc_private_test_directory(path string) ! {
	stat := os.lstat(path)!
	assert stat.get_filetype() == .directory
	assert stat.uid == u32(os.geteuid())
	assert stat.mode & 0o7777 == 0o700
}

fn create_tcc_macos_libgc_test_home(root string, name string) !string {
	home := os.join_path(root, name)
	os.mkdir(home, mode: 0o700)!
	return home
}

fn expect_tcc_macos_libgc_home_fallback_error(home string, rejected string, expected_reason string, absent_path string) {
	expect_tcc_macos_libgc_data_base_error('', home, rejected, expected_reason)
	assert !os.exists(absent_path)
}

fn restore_tcc_macos_libgc_test_environment(name string, old_value ?string) {
	if value := old_value {
		os.setenv(name, value, true)
	} else {
		os.unsetenv(name)
	}
}

fn resolve_tcc_macos_libgc_home_fallback_concurrently(home string, start chan bool) !string {
	_ := <-start
	return resolve_tcc_macos_libgc_data_base('', home)
}

fn prepare_tcc_macos_libgc_test_content_store(fixture TccMacosLibgcTestFixture) !TccMacosLibgcTestContentStore {
	resolved_source := resolve_tcc_macos_libgc_source(fixture.libgc.source)!
	expected := hash_tcc_macos_libgc_regular_file(resolved_source)!
	store_root := os.join_path(fixture.data_base, tcc_macos_libgc_store_name)
	ensure_tcc_macos_libgc_private_directory(store_root)!
	content_dir := os.join_path(store_root, expected.sum)
	ensure_tcc_macos_libgc_private_directory(content_dir)!
	return TccMacosLibgcTestContentStore{
		resolved_source: resolved_source
		expected:        expected
		store_root:      store_root
		content_dir:     content_dir
		final_dylib:     os.join_path(content_dir, tcc_macos_libgc_name)
	}
}

fn test_tcc_macos_libgc_planner_ignores_absence() {
	vroot := '/Applications/V, active domain'
	linker_flags := ['-lm', '"/opt/vendor/libother.dylib"']
	pre_args := ['-Wl,-rpath,"/opt/vendor/lib"', '-DAPP_FLAG=1']
	plan := plan_tcc_macos_libgc_store(vroot, linker_flags, pre_args)!
	assert !plan.required
	assert linker_flags == ['-lm', '"/opt/vendor/libother.dylib"']
	assert pre_args == ['-Wl,-rpath,"/opt/vendor/lib"', '-DAPP_FLAG=1']
}

fn test_tcc_macos_libgc_planner_keeps_exact_pair_without_comma() {
	vroot := '/Applications/V stable'
	dylib, rpath := tcc_macos_libgc_test_tokens(vroot)
	plan := plan_tcc_macos_libgc_store(vroot, [dylib], [rpath])!
	assert !plan.required
}

fn test_tcc_macos_libgc_planner_noops_before_rejecting_out_of_scope_flags_without_comma() {
	vroot := '/Applications/V stable'
	dylib, rpath := tcc_macos_libgc_test_tokens(vroot)
	linker_flags := [dylib, dylib, dylib.trim('"'), '"/other/thirdparty/tcc/lib/libgc.dylib"']
	pre_args := [rpath, rpath, rpath + '-near-match']
	plan := plan_tcc_macos_libgc_store(vroot, linker_flags, pre_args)!
	assert !plan.required
	assert linker_flags == [dylib, dylib, dylib.trim('"'), '"/other/thirdparty/tcc/lib/libgc.dylib"']
	assert pre_args == [rpath, rpath, rpath + '-near-match']
}

fn test_tcc_macos_libgc_planner_uses_physical_vroot_for_comma_activation() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(),
		'v_builder_tcc_macos_libgc_${os.getpid()}_physical_activation')
	os.rmdir_all(root) or {}
	os.mkdir_all(root, mode: 0o700)!
	defer {
		os.rmdir_all(root) or {}
	}
	physical_without_comma := os.join_path(root, 'physical vroot')
	os.mkdir(physical_without_comma, mode: 0o700)!
	lexical_with_comma := os.join_path(root, 'lexical,vroot')
	os.symlink(physical_without_comma, lexical_with_comma)!
	no_op_dylib, no_op_rpath := tcc_macos_libgc_test_tokens(lexical_with_comma)
	no_op_plan := plan_tcc_macos_libgc_store(lexical_with_comma, [no_op_dylib, no_op_dylib,
		no_op_dylib.trim('"')], [no_op_rpath, no_op_rpath + '-near-match'])!
	assert !no_op_plan.required
	physical_with_comma := os.join_path(root, 'physical,vroot')
	os.mkdir(physical_with_comma, mode: 0o700)!
	lexical_without_comma := os.join_path(root, 'lexical-vroot')
	os.symlink(physical_with_comma, lexical_without_comma)!
	active_dylib, active_rpath := tcc_macos_libgc_test_tokens(lexical_without_comma)
	active_plan := plan_tcc_macos_libgc_store(lexical_without_comma, [active_dylib], [
		active_rpath,
	])!
	assert active_plan.required
}

fn test_tcc_macos_libgc_planner_requires_one_complete_pair() {
	vroot := '/Applications/V, nightly'
	dylib, rpath := tcc_macos_libgc_test_tokens(vroot)
	expect_tcc_macos_libgc_plan_error(vroot, [dylib], [], 'expected one exact')
	expect_tcc_macos_libgc_plan_error(vroot, [], [rpath], 'expected one exact')
	expect_tcc_macos_libgc_plan_error(vroot, [dylib, dylib], [rpath], 'found 2/1')
	expect_tcc_macos_libgc_plan_error(vroot, [dylib], [rpath, rpath], 'found 1/2')
	expect_tcc_macos_libgc_plan_error(vroot, [dylib, rpath], [dylib], 'found 2/1')
	expect_tcc_macos_libgc_plan_error(vroot, [dylib, rpath], [rpath], 'found 1/2')
}

fn test_tcc_macos_libgc_planner_rejects_near_matches() {
	vroot := '/Applications/V, nightly'
	dylib, rpath := tcc_macos_libgc_test_tokens(vroot)
	unquoted_dylib := dylib.trim('"')
	expect_tcc_macos_libgc_plan_error(vroot, [unquoted_dylib], [], 'near-match')
	expect_tcc_macos_libgc_plan_error(vroot, [dylib + '.backup'], [rpath], 'near-match')
	expect_tcc_macos_libgc_plan_error(vroot, [dylib], [rpath + '-other'], 'near-match')
	expect_tcc_macos_libgc_plan_error(vroot, [], ['  ${rpath}  '], 'near-match')
	expect_tcc_macos_libgc_plan_error(vroot, [], [
		rpath.replace('-Wl,-rpath,', '-Wl,--rpath='),
	], 'near-match')
	expect_tcc_macos_libgc_plan_error(vroot, [dylib, '"/other/thirdparty/tcc/lib/libgc.dylib"'], [
		rpath,
	], 'near-match')
}

fn test_tcc_macos_libgc_rewrite_preserves_third_party_rpaths_byte_for_byte() {
	vroot := '/Applications/V checkout, nightly build'
	dylib, rpath := tcc_macos_libgc_test_tokens(vroot)
	linker_flags := ['-lm', dylib, '"/opt/vendor/libother.dylib"']
	pre_args := [
		'-Wl,-rpath,"/opt/vendor/lib"',
		rpath,
		'-Wl,-rpath,"@loader_path/../Frameworks"',
	]
	plan := plan_tcc_macos_libgc_store(vroot, linker_flags, pre_args)!
	assert plan.required
	hash := 'a'.repeat(64)
	stored_dylib := '/Users/test/Library/Application Support/v-tcc-libgc-v1/${hash}/libgc.dylib'
	rewritten_linker_flags, rewritten_pre_args := rewrite_tcc_macos_libgc_flags(plan, linker_flags,
		pre_args, stored_dylib)!
	assert rewritten_linker_flags == [
		'-lm',
		'"${stored_dylib}"',
		'"/opt/vendor/libother.dylib"',
	]
	assert rewritten_pre_args == [
		'-Wl,-rpath,"/opt/vendor/lib"',
		'-Wl,-rpath,"${os.dir(stored_dylib)}"',
		'-Wl,-rpath,"@loader_path/../Frameworks"',
	]
	assert linker_flags == ['-lm', dylib, '"/opt/vendor/libother.dylib"']
	assert pre_args[0] == '-Wl,-rpath,"/opt/vendor/lib"'
	assert pre_args[2] == '-Wl,-rpath,"@loader_path/../Frameworks"'
}

fn test_tcc_macos_libgc_rewrite_is_transactional_on_error() {
	vroot := '/Applications/V checkout, nightly build'
	dylib, rpath := tcc_macos_libgc_test_tokens(vroot)
	plan := plan_tcc_macos_libgc_store(vroot, [dylib], [rpath])!
	linker_flags := ['"tampered-after-plan.dylib"']
	pre_args := [rpath]
	original_linker_flags := linker_flags.clone()
	original_pre_args := pre_args.clone()
	rewrite_tcc_macos_libgc_flags(plan, linker_flags, pre_args,
		'/Users/test/data/store/hash/libgc.dylib') or {
		assert err.msg().contains('changed after planning'), err.msg()
		assert linker_flags == original_linker_flags
		assert pre_args == original_pre_args
		return
	}
	assert false, 'expected a stale macOS bundled libgc plan to fail'
}

fn test_tcc_macos_libgc_materialize_and_rewrite_keeps_inputs_on_materialization_error() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('transactional_materialize',
		'transactional-materialize-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	vroot := os.dir(os.dir(os.dir(fixture.libgc.lib_dir)))
	dylib, rpath := tcc_macos_libgc_test_tokens(vroot)
	linker_flags := ['-lm', dylib, '-lthird_party']
	pre_args := ['-DTRANSACTIONAL_SENTINEL=1', rpath]
	original_linker_flags := linker_flags.clone()
	original_pre_args := pre_args.clone()
	plan := plan_tcc_macos_libgc_store(vroot, linker_flags, pre_args)!
	assert plan.required
	comma_base := os.join_path(fixture.root, 'transactional,persistent-data')
	os.mkdir(comma_base, mode: 0o700)!
	materialize_and_rewrite_tcc_macos_libgc_flags_at(plan, linker_flags, pre_args, comma_base) or {
		assert err.msg().contains(comma_base), err.msg()
		assert linker_flags == original_linker_flags
		assert pre_args == original_pre_args
		assert !os.exists(os.join_path(comma_base, tcc_macos_libgc_store_name))
		return
	}
	assert false, 'expected materialization failure to leave compiler arguments unchanged'
}

fn test_tcc_macos_libgc_rewrite_supports_ordered_pkgconfig_layout() {
	vroot := '/Applications/V checkout, ordered pkgconfig'
	dylib, rpath := tcc_macos_libgc_test_tokens(vroot)
	linker_flags := [
		'-Wl,-rpath,"/opt/vendor/lib"',
		dylib,
		'-lthird_party',
		rpath,
	]
	pre_args := ['-DORDERED_PKGCONFIG_SENTINEL=1']
	plan := plan_tcc_macos_libgc_store(vroot, linker_flags, pre_args)!
	assert plan.required
	hash := 'c'.repeat(64)
	stored_dylib := '/Users/test/data/v-tcc-libgc-v1/${hash}/libgc.dylib'
	rewritten_linker_flags, rewritten_pre_args := rewrite_tcc_macos_libgc_flags(plan, linker_flags,
		pre_args, stored_dylib)!
	assert rewritten_linker_flags == [
		'-Wl,-rpath,"/opt/vendor/lib"',
		'"${stored_dylib}"',
		'-lthird_party',
		'-Wl,-rpath,"${os.dir(stored_dylib)}"',
	]
	assert rewritten_pre_args == pre_args
	assert linker_flags[0] == '-Wl,-rpath,"/opt/vendor/lib"'
	assert linker_flags[2] == '-lthird_party'
}

fn test_tcc_macos_libgc_planner_is_architecture_neutral() {
	vroot := '/Applications/V arm64, nightly'
	dylib, rpath := tcc_macos_libgc_test_tokens(vroot)
	linker_flags := ['-arch', 'arm64', dylib]
	pre_args := ['-DARM64_SENTINEL=1', rpath]
	plan := plan_tcc_macos_libgc_store(vroot, linker_flags, pre_args)!
	assert plan.required
	hash := 'b'.repeat(64)
	stored_dylib := '/Users/test/data/v-tcc-libgc-v1/${hash}/libgc.dylib'
	rewritten_linker_flags, rewritten_pre_args := rewrite_tcc_macos_libgc_flags(plan, linker_flags,
		pre_args, stored_dylib)!
	assert rewritten_linker_flags[..2] == ['-arch', 'arm64']
	assert rewritten_pre_args[0] == '-DARM64_SENTINEL=1'
}

fn test_tcc_macos_libgc_data_base_resolver_is_fallible_and_absolute() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('data_base', 'resolver-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	assert resolve_tcc_macos_libgc_data_base(fixture.data_base, '')! == os.real_path(fixture.data_base)
	expect_tcc_macos_libgc_data_base_error('relative/data', '', 'relative/data', 'relative')
	expect_tcc_macos_libgc_data_base_error('', '', 'XDG_DATA_HOME="" and HOME=""',
		'no persistent data root')
}

fn test_tcc_macos_libgc_data_base_resolver_creates_and_materializes_home_fallback() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('home_fallback', 'home-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	home := create_tcc_macos_libgc_test_home(fixture.root, 'home')!
	fallback := os.join_path(home, '.local', 'share')
	assert !os.exists(os.join_path(home, '.local'))
	old_xdg_data_home := os.getenv_opt('XDG_DATA_HOME')
	old_home := os.getenv_opt('HOME')
	defer {
		restore_tcc_macos_libgc_test_environment('XDG_DATA_HOME', old_xdg_data_home)
		restore_tcc_macos_libgc_test_environment('HOME', old_home)
	}
	os.setenv('XDG_DATA_HOME', '', true)
	os.setenv('HOME', home, true)
	stored := materialize_tcc_macos_libgc(fixture.libgc.source)!
	resolved := os.real_path(fallback)
	assert os.dir(os.dir(os.dir(stored))) == resolved
	assert_tcc_macos_libgc_private_test_directory(os.join_path(home, '.local'))!
	assert_tcc_macos_libgc_private_test_directory(fallback)!
	assert stored.starts_with(os.join_path(resolved, tcc_macos_libgc_store_name) + os.path_separator)
	assert os.read_file(stored)! == os.read_file(fixture.libgc.target)!
}

fn test_tcc_macos_libgc_data_base_home_fallback_creation_is_idempotent_under_concurrency() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('home_fallback_concurrent',
		'home-concurrent-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	home := create_tcc_macos_libgc_test_home(fixture.root, 'home')!
	worker_count := 8
	start := chan bool{cap: worker_count}
	mut workers := []thread !string{cap: worker_count}
	for _ in 0 .. worker_count {
		workers << spawn resolve_tcc_macos_libgc_home_fallback_concurrently(home, start)
	}
	for _ in 0 .. worker_count {
		start <- true
	}
	fallback := os.join_path(home, '.local', 'share')
	for worker in workers {
		assert worker.wait()! == os.real_path(fallback)
	}
	assert_tcc_macos_libgc_private_test_directory(os.join_path(home, '.local'))!
	assert_tcc_macos_libgc_private_test_directory(fallback)!
}

fn test_tcc_macos_libgc_data_base_home_fallback_preserves_private_symlink() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('home_fallback_symlink', 'home-symlink-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	home := create_tcc_macos_libgc_test_home(fixture.root, 'home')!
	local_target := os.join_path(fixture.root, 'private local data')
	os.mkdir(local_target, mode: 0o700)!
	os.symlink(local_target, os.join_path(home, '.local'))!
	fallback_target := os.join_path(local_target, 'share')
	assert resolve_tcc_macos_libgc_data_base('', home)! == os.real_path(fallback_target)
	assert os.is_link(os.join_path(home, '.local'))
	assert_tcc_macos_libgc_private_test_directory(fallback_target)!
}

fn test_tcc_macos_libgc_data_base_home_fallback_rejects_unsafe_traversal() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('home_fallback_unsafe', 'unsafe-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	unsafe_home := create_tcc_macos_libgc_test_home(fixture.root, 'unsafe home')!
	os.chmod(unsafe_home, 0o770)!
	expect_tcc_macos_libgc_home_fallback_error(unsafe_home, unsafe_home, 'unsafe permissions', os.join_path(unsafe_home,
		'.local'))

	file_home := create_tcc_macos_libgc_test_home(fixture.root, 'file home')!
	file_local := os.join_path(file_home, '.local')
	os.write_file(file_local, 'not-a-directory')!
	expect_tcc_macos_libgc_home_fallback_error(file_home, file_local, 'not a directory', os.join_path(file_local,
		'share'))

	writable_home := create_tcc_macos_libgc_test_home(fixture.root, 'writable local home')!
	writable_local := os.join_path(writable_home, '.local')
	os.mkdir(writable_local, mode: 0o700)!
	os.chmod(writable_local, 0o770)!
	expect_tcc_macos_libgc_home_fallback_error(writable_home, writable_local, 'unsafe permissions', os.join_path(writable_local,
		'share'))

	unsafe_share_home := create_tcc_macos_libgc_test_home(fixture.root, 'unsafe share home')!
	unsafe_share_local := os.join_path(unsafe_share_home, '.local')
	unsafe_share := os.join_path(unsafe_share_local, 'share')
	os.mkdir(unsafe_share_local, mode: 0o700)!
	os.mkdir(unsafe_share, mode: 0o700)!
	os.chmod(unsafe_share, 0o770)!
	expect_tcc_macos_libgc_home_fallback_error(unsafe_share_home, unsafe_share,
		'unsafe permissions', os.join_path(unsafe_share, tcc_macos_libgc_store_name))

	unsafe_target_home := create_tcc_macos_libgc_test_home(fixture.root, 'unsafe target home')!
	unsafe_target := os.join_path(fixture.root, 'unsafe local target')
	os.mkdir(unsafe_target, mode: 0o700)!
	os.chmod(unsafe_target, 0o770)!
	os.symlink(unsafe_target, os.join_path(unsafe_target_home, '.local'))!
	expect_tcc_macos_libgc_home_fallback_error(unsafe_target_home, unsafe_target,
		'unsafe permissions', os.join_path(unsafe_target, 'share'))

	comma_target_home := create_tcc_macos_libgc_test_home(fixture.root, 'comma target home')!
	comma_target := os.join_path(fixture.root, 'private,local target')
	os.mkdir(comma_target, mode: 0o700)!
	os.symlink(comma_target, os.join_path(comma_target_home, '.local'))!
	expect_tcc_macos_libgc_home_fallback_error(comma_target_home, comma_target, 'comma-free', os.join_path(comma_target,
		'share'))
}

fn test_tcc_macos_libgc_data_base_resolver_rejects_absence() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('missing_data_base', 'missing-base-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	missing_base := os.join_path(fixture.root, 'missing persistent data root')
	expect_tcc_macos_libgc_data_base_error(missing_base, '', missing_base, 'does not exist')
	assert !os.exists(missing_base)
}

fn test_tcc_macos_libgc_data_base_accepts_owned_0755() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('base_0755', 'base-0755-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	os.chmod(fixture.data_base, 0o755)!
	assert resolve_tcc_macos_libgc_data_base(fixture.data_base, '')! == os.real_path(fixture.data_base)
}

fn test_tcc_macos_libgc_data_base_rejects_unsafe_modes() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('base_modes', 'base-mode-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	os.chmod(fixture.data_base, 0o775)!
	expect_tcc_macos_libgc_data_base_error(fixture.data_base, '', fixture.data_base,
		'unsafe permissions')
	os.chmod(fixture.data_base, 0o1755)!
	expect_tcc_macos_libgc_data_base_error(fixture.data_base, '', fixture.data_base,
		'unsafe permissions')
}

fn test_tcc_macos_libgc_data_base_rejects_non_directory() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('base_type', 'base-type-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	file_base := os.join_path(fixture.root, 'data-root-file')
	os.write_file(file_base, 'not-a-directory')!
	expect_tcc_macos_libgc_data_base_error(file_base, '', file_base, 'not a directory')
}

fn test_tcc_macos_libgc_data_base_owner_validation_is_actionable() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('base_owner', 'base-owner-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	stat := os.lstat(fixture.data_base)!
	validate_tcc_macos_libgc_data_base_stat(stat, fixture.data_base, os.geteuid() + 1) or {
		assert err.msg().contains(fixture.data_base), err.msg()
		assert err.msg().contains('not owned'), err.msg()
		assert err.msg().contains(tcc_macos_libgc_data_root_remediation), err.msg()
		return
	}
	assert false, 'expected a data root owned by another uid to fail validation'
}

fn test_tcc_macos_libgc_store_rejects_comma_data_root() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('comma_root', 'comma-root-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	comma_base := os.join_path(fixture.root, 'persistent,data')
	os.mkdir(comma_base, mode: 0o700)!
	materialize_tcc_macos_libgc_at(fixture.libgc.source, comma_base) or {
		message := err.msg()
		assert message.contains(comma_base), message
		assert message.contains(tcc_macos_libgc_data_root_remediation), message
		assert message.contains('comma-free'), message
		assert !os.exists(os.join_path(comma_base, tcc_macos_libgc_store_name))
		return
	}
	assert false, 'expected a comma data root to fail'
}

fn test_tcc_macos_libgc_store_rejects_empty_source_without_creating_store() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('empty_source', '')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	expect_tcc_macos_libgc_materialize_error(fixture.libgc.source, fixture.data_base,
		'outside the accepted range')
	assert !os.exists(os.join_path(fixture.data_base, tcc_macos_libgc_store_name))
}

fn test_tcc_macos_libgc_store_rejects_oversized_sparse_source_without_creating_store() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('oversized_source', 'seed')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	os.truncate(fixture.libgc.target, tcc_macos_libgc_max_size + 1)!
	expect_tcc_macos_libgc_materialize_error(fixture.libgc.source, fixture.data_base,
		'outside the accepted range')
	assert !os.exists(os.join_path(fixture.data_base, tcc_macos_libgc_store_name))
}

fn test_tcc_macos_libgc_store_materializes_byte_identical_regular_object() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('healthy', 'healthy-content-123456')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	source_before := os.read_bytes(fixture.libgc.target)!
	stored := materialize_tcc_macos_libgc_at(fixture.libgc.source, fixture.data_base)!
	assert os.read_bytes(stored)! == source_before
	assert os.read_bytes(fixture.libgc.target)! == source_before
	stat := os.lstat(stored)!
	assert stat.get_filetype() == .regular
	assert stat.get_mode().bitmask() == 0o700
	assert stat.uid == u32(os.geteuid())
	assert os.file_name(os.dir(stored)).len == 64
	assert os.ls(os.dir(stored))! == [tcc_macos_libgc_name]
}

fn test_tcc_macos_libgc_store_reuses_same_content_key() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('same_key', 'same-content-key')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	second_source := create_tcc_macos_libgc_test_source(fixture.root, 'secondary',
		'same-content-key')!
	os.utime(fixture.libgc.target, i64(1_900_000_000), i64(1_900_000_000))!
	os.utime(second_source.target, i64(2_000_000_000), i64(2_000_000_000))!
	assert os.file_last_mod_unix(fixture.libgc.target) != os.file_last_mod_unix(second_source.target)
	first := materialize_tcc_macos_libgc_at(fixture.libgc.source, fixture.data_base)!
	second := materialize_tcc_macos_libgc_at(second_source.source, fixture.data_base)!
	assert first == second
}

fn test_tcc_macos_libgc_store_keys_bytes_not_mtime() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('mtime_key', 'content-alpha')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	second_source := create_tcc_macos_libgc_test_source(fixture.root, 'secondary', 'content-bravo')!
	mtime := i64(2_000_000_000)
	os.utime(fixture.libgc.target, mtime, mtime)!
	os.utime(second_source.target, mtime, mtime)!
	first := materialize_tcc_macos_libgc_at(fixture.libgc.source, fixture.data_base)!
	second := materialize_tcc_macos_libgc_at(second_source.source, fixture.data_base)!
	assert os.file_last_mod_unix(fixture.libgc.target) == os.file_last_mod_unix(second_source.target)
	assert first != second
}

fn test_tcc_macos_libgc_store_refuses_truncated_final_without_repair() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('truncated', 'complete-content-for-truncation')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	stored := materialize_tcc_macos_libgc_at(fixture.libgc.source, fixture.data_base)!
	os.write_file(stored, 'x')!
	expect_tcc_macos_libgc_materialize_error(fixture.libgc.source, fixture.data_base,
		'content hash validation')
	assert os.read_file(stored)! == 'x'
}

fn test_tcc_macos_libgc_store_refuses_wrong_hash_without_repair() ! {
	$if windows {
		return
	}
	content := 'equal-length-original-content'
	fixture := create_tcc_macos_libgc_test_fixture('wrong_hash', content)!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	stored := materialize_tcc_macos_libgc_at(fixture.libgc.source, fixture.data_base)!
	original_mtime := os.file_last_mod_unix(stored)
	wrong := 'x'.repeat(content.len)
	os.write_file(stored, wrong)!
	os.utime(stored, original_mtime, original_mtime)!
	assert os.file_last_mod_unix(stored) == original_mtime
	expect_tcc_macos_libgc_materialize_error(fixture.libgc.source, fixture.data_base,
		'content hash validation')
	assert os.read_file(stored)! == wrong
}

fn test_tcc_macos_libgc_store_refuses_symlink_final() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('symlink_final', 'symlink-final-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	stored := materialize_tcc_macos_libgc_at(fixture.libgc.source, fixture.data_base)!
	os.rm(stored)!
	os.symlink(fixture.libgc.target, stored)!
	expect_tcc_macos_libgc_materialize_error(fixture.libgc.source, fixture.data_base,
		'not a regular file')
	assert os.lstat(stored)!.get_filetype() == .symbolic_link
}

fn test_tcc_macos_libgc_store_refuses_wrong_final_mode() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('wrong_mode', 'wrong-mode-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	stored := materialize_tcc_macos_libgc_at(fixture.libgc.source, fixture.data_base)!
	os.chmod(stored, 0o755)!
	expect_tcc_macos_libgc_materialize_error(fixture.libgc.source, fixture.data_base, 'mode 0700')
	assert os.lstat(stored)!.get_mode().bitmask() == 0o755
}

fn test_tcc_macos_libgc_store_object_owner_validation_fails_closed() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('wrong_owner', 'wrong-owner-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	stored := materialize_tcc_macos_libgc_at(fixture.libgc.source, fixture.data_base)!
	stat := os.lstat(stored)!
	validate_tcc_macos_libgc_store_object_stat(stat, os.geteuid() + 1) or {
		assert err.msg().contains('not owned'), err.msg()
		return
	}
	assert false, 'expected a store object owned by another uid to fail validation'
}

fn test_tcc_macos_libgc_store_refuses_hostile_leaf() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('hostile_leaf', 'hostile-leaf-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	outside := os.join_path(fixture.root, 'outside store')
	os.mkdir(outside, mode: 0o700)!
	store_root := os.join_path(fixture.data_base, tcc_macos_libgc_store_name)
	os.symlink(outside, store_root)!
	expect_tcc_macos_libgc_materialize_error(fixture.libgc.source, fixture.data_base,
		'not a directory')
	assert os.lstat(store_root)!.get_filetype() == .symbolic_link
}

fn test_tcc_macos_libgc_store_refuses_leaf_with_wrong_mode() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('leaf_mode', 'leaf-mode-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	store_root := os.join_path(fixture.data_base, tcc_macos_libgc_store_name)
	os.mkdir(store_root, mode: 0o700)!
	os.chmod(store_root, 0o755)!
	expect_tcc_macos_libgc_materialize_error(fixture.libgc.source, fixture.data_base, 'mode 0700')
}

fn test_tcc_macos_libgc_store_leaf_owner_validation_fails_closed() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('leaf_owner', 'leaf-owner-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	store_root := os.join_path(fixture.data_base, tcc_macos_libgc_store_name)
	os.mkdir(store_root, mode: 0o700)!
	stat := os.lstat(store_root)!
	validate_tcc_macos_libgc_private_directory_stat(stat, os.geteuid() + 1) or {
		assert err.msg().contains('not owned'), err.msg()
		return
	}
	assert false, 'expected a store leaf owned by another uid to fail validation'
}

fn test_tcc_macos_libgc_store_refuses_content_directory_symlink() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('content_symlink', 'content-symlink-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	resolved_source := resolve_tcc_macos_libgc_source(fixture.libgc.source)!
	expected := hash_tcc_macos_libgc_regular_file(resolved_source)!
	store_root := os.join_path(fixture.data_base, tcc_macos_libgc_store_name)
	os.mkdir(store_root, mode: 0o700)!
	outside := os.join_path(fixture.root, 'outside content directory')
	os.mkdir(outside, mode: 0o700)!
	content_dir := os.join_path(store_root, expected.sum)
	os.symlink(outside, content_dir)!
	expect_tcc_macos_libgc_materialize_error(fixture.libgc.source, fixture.data_base,
		'not a directory')
	assert os.lstat(content_dir)!.get_filetype() == .symbolic_link
	assert os.ls(outside)! == []
}

fn test_tcc_macos_libgc_store_refuses_content_directory_regular_file() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('content_file', 'content-file-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	resolved_source := resolve_tcc_macos_libgc_source(fixture.libgc.source)!
	expected := hash_tcc_macos_libgc_regular_file(resolved_source)!
	store_root := os.join_path(fixture.data_base, tcc_macos_libgc_store_name)
	os.mkdir(store_root, mode: 0o700)!
	content_dir := os.join_path(store_root, expected.sum)
	os.write_file(content_dir, 'hostile-content-directory-file')!
	expect_tcc_macos_libgc_materialize_error(fixture.libgc.source, fixture.data_base,
		'not a directory')
	assert os.read_file(content_dir)! == 'hostile-content-directory-file'
}

fn test_tcc_macos_libgc_store_refuses_content_directory_with_wrong_mode() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('content_mode', 'content-mode-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	resolved_source := resolve_tcc_macos_libgc_source(fixture.libgc.source)!
	expected := hash_tcc_macos_libgc_regular_file(resolved_source)!
	store_root := os.join_path(fixture.data_base, tcc_macos_libgc_store_name)
	os.mkdir(store_root, mode: 0o700)!
	content_dir := os.join_path(store_root, expected.sum)
	os.mkdir(content_dir, mode: 0o700)!
	os.chmod(content_dir, 0o755)!
	expect_tcc_macos_libgc_materialize_error(fixture.libgc.source, fixture.data_base, 'mode 0700')
	assert os.lstat(content_dir)!.get_mode().bitmask() == 0o755
}

fn test_tcc_macos_libgc_source_symlink_must_remain_internal() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('external_source', 'internal-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	external_target := os.join_path(fixture.root, 'external-libgc.dylib')
	os.write_file(external_target, 'external-content')!
	os.chmod(external_target, 0o700)!
	os.rm(fixture.libgc.source)!
	os.symlink(external_target, fixture.libgc.source)!
	expect_tcc_macos_libgc_materialize_error(fixture.libgc.source, fixture.data_base,
		'resolves outside')
}

fn test_tcc_macos_libgc_source_accepts_official_internal_symlink() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('internal_source', 'internal-source-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	assert os.lstat(fixture.libgc.source)!.get_filetype() == .symbolic_link
	assert resolve_tcc_macos_libgc_source(fixture.libgc.source)! == os.real_path(fixture.libgc.target)
}

fn test_tcc_macos_libgc_source_parent_symlink_must_remain_inside_canonical_vroot() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_builder_tcc_macos_libgc_${os.getpid()}_external_parent')
	os.rmdir_all(root) or {}
	os.mkdir_all(root, mode: 0o700)!
	defer {
		os.rmdir_all(root) or {}
	}
	data_base := os.join_path(root, 'persistent data root')
	os.mkdir(data_base, mode: 0o700)!
	lexical_vroot := os.join_path(root, 'symlinked V root, with comma')
	lexical_tcc_dir := os.join_path(lexical_vroot, 'thirdparty', 'tcc')
	os.mkdir_all(lexical_tcc_dir, mode: 0o700)!
	external_lib_dir := os.join_path(root, 'external parent', 'lib')
	os.mkdir_all(external_lib_dir, mode: 0o700)!
	external_source := os.join_path(external_lib_dir, tcc_macos_libgc_name)
	os.write_file(external_source, 'external-parent-content')!
	os.chmod(external_source, 0o700)!
	lexical_lib_dir := os.join_path(lexical_tcc_dir, 'lib')
	os.symlink(external_lib_dir, lexical_lib_dir)!
	lexical_source := os.join_path(lexical_lib_dir, tcc_macos_libgc_name)
	dylib, rpath := tcc_macos_libgc_test_tokens(lexical_vroot)
	linker_flags := [dylib]
	pre_args := [rpath]
	plan := plan_tcc_macos_libgc_store(lexical_vroot, linker_flags, pre_args)!
	assert plan.required
	assert plan.source_dylib == lexical_source
	materialize_and_rewrite_tcc_macos_libgc_flags_at(plan, linker_flags, pre_args, data_base) or {
		assert err.msg().contains('directory resolves outside the canonical VROOT'), err.msg()
		assert os.lstat(lexical_lib_dir)!.get_filetype() == .symbolic_link
		assert os.read_file(external_source)! == 'external-parent-content'
		assert !os.exists(os.join_path(data_base, tcc_macos_libgc_store_name))
		return
	}
	assert false, 'expected an external source parent symlink to fail materialization'
}

fn test_tcc_macos_libgc_copy_skips_hostile_temp_name_collisions_and_cleans_its_object() ! {
	$if windows {
		return
	}
	fixture :=
		create_tcc_macos_libgc_test_fixture('temp_collisions', 'temporary-collision-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	store := prepare_tcc_macos_libgc_test_content_store(fixture)!
	file_collision := tcc_macos_libgc_temp_directory_path(store.content_dir, 0)
	os.write_file(file_collision, 'preserve-file-collision')!
	symlink_collision := tcc_macos_libgc_temp_directory_path(store.content_dir, 1)
	symlink_target := os.join_path(fixture.root, 'preserve symlink target')
	os.write_file(symlink_target, 'preserve-symlink-target')!
	os.symlink(symlink_target, symlink_collision)!
	temporary := copy_tcc_macos_libgc_to_exclusive_temp(store.resolved_source, store.content_dir,
		store.expected)!
	assert temporary.directory != file_collision
	assert temporary.directory != symlink_collision
	cleanup_tcc_macos_libgc_temporary(temporary)!
	assert !os.exists(temporary.path)
	assert !os.exists(temporary.directory)
	assert os.read_file(file_collision)! == 'preserve-file-collision'
	assert os.lstat(symlink_collision)!.get_filetype() == .symbolic_link
	actual_symlink_target := os.readlink(symlink_collision)!
	assert actual_symlink_target == symlink_target
}

fn test_tcc_macos_libgc_copy_error_removes_its_temporary_directory() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('copy_error_cleanup',
		'copy-error-cleanup-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	store := prepare_tcc_macos_libgc_test_content_store(fixture)!
	short_expected := TccMacosLibgcFileHash{
		sum:  store.expected.sum
		size: store.expected.size - 1
	}
	copy_tcc_macos_libgc_to_exclusive_temp(store.resolved_source, store.content_dir, short_expected) or {
		assert err.msg().contains('grew beyond the accepted copy size'), err.msg()
		assert os.ls(store.content_dir)! == []
		return
	}
	assert false, 'expected bounded streaming copy failure to clean its temporary directory'
}

fn test_tcc_macos_libgc_post_close_hash_error_removes_its_temporary_directory() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('post_close_cleanup',
		'post-close-cleanup-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	store := prepare_tcc_macos_libgc_test_content_store(fixture)!
	wrong_expected := TccMacosLibgcFileHash{
		sum:  '0'.repeat(64)
		size: store.expected.size
	}
	copy_tcc_macos_libgc_to_exclusive_temp(store.resolved_source, store.content_dir, wrong_expected) or {
		assert err.msg().contains('post-close validation'), err.msg()
		assert os.ls(store.content_dir)! == []
		return
	}
	assert false, 'expected post-close hash failure to clean its temporary directory'
}

fn test_tcc_macos_libgc_cleanup_propagates_rmdir_failure_after_removing_exact_file() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('cleanup_error', 'cleanup-error-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	store := prepare_tcc_macos_libgc_test_content_store(fixture)!
	temporary := copy_tcc_macos_libgc_to_exclusive_temp(store.resolved_source, store.content_dir,
		store.expected)!
	unexpected := os.join_path(temporary.directory, 'unexpected-object')
	os.write_file(unexpected, 'preserve-unexpected-object')!
	cleanup_tcc_macos_libgc_temporary(temporary) or {
		assert err.msg().contains('cannot remove macOS bundled libgc temporary directory'), err.msg()
		assert !os.exists(temporary.path)
		assert os.exists(temporary.directory)
		assert os.read_file(unexpected)! == 'preserve-unexpected-object'
		return
	}
	assert false, 'expected exact cleanup to propagate a non-empty directory failure'
}

fn test_tcc_macos_libgc_publication_propagates_cleanup_failure_without_removing_unknown_file() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('publication_cleanup_error',
		'publication-cleanup-error-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	store := prepare_tcc_macos_libgc_test_content_store(fixture)!
	temporary := copy_tcc_macos_libgc_to_exclusive_temp(store.resolved_source, store.content_dir,
		store.expected)!
	unknown := os.join_path(temporary.directory, 'unknown-sentinel')
	os.write_file(unknown, 'preserve-unknown-sentinel')!
	publish_tcc_macos_libgc_temporary(temporary, store.final_dylib, store.expected) or {
		assert err.msg().contains('final object is valid, but temporary cleanup failed'), err.msg()
		validate_tcc_macos_libgc_store_object(store.final_dylib, store.expected)!
		assert !os.exists(temporary.path)
		assert os.exists(temporary.directory)
		assert os.read_file(unknown)! == 'preserve-unknown-sentinel'
		return
	}
	assert false, 'expected publication to propagate exact cleanup failure'
}

fn publish_tcc_macos_libgc_concurrently(temporary TccMacosLibgcTemporaryObject, final_dylib string, expected TccMacosLibgcFileHash, start chan bool) !TccMacosLibgcPublicationResult {
	_ := <-start
	return publish_tcc_macos_libgc_temporary(temporary, final_dylib, expected)
}

fn test_tcc_macos_libgc_store_publication_has_one_causal_winner_and_no_temporary() ! {
	$if windows {
		return
	}
	fixture := create_tcc_macos_libgc_test_fixture('concurrent', 'concurrent-content')!
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	store := prepare_tcc_macos_libgc_test_content_store(fixture)!
	worker_count := 8
	mut temporaries := []TccMacosLibgcTemporaryObject{cap: worker_count}
	for _ in 0 .. worker_count {
		temporary := copy_tcc_macos_libgc_to_exclusive_temp(store.resolved_source,
			store.content_dir, store.expected)!
		assert os.exists(temporary.directory)
		assert os.exists(temporary.path)
		temporaries << temporary
	}
	assert !os.exists(store.final_dylib)
	start := chan bool{cap: worker_count}
	mut workers := []thread !TccMacosLibgcPublicationResult{cap: worker_count}
	for temporary in temporaries {
		workers << spawn publish_tcc_macos_libgc_concurrently(temporary, store.final_dylib,
			store.expected, start)
	}
	for _ in 0 .. worker_count {
		start <- true
	}
	mut results := []TccMacosLibgcPublicationResult{cap: worker_count}
	for worker in workers {
		results << worker.wait()!
	}
	mut winners := 0
	for result in results {
		assert result.final_dylib == store.final_dylib
		if result.won {
			winners++
		}
	}
	assert winners == 1
	assert results.len - winners == worker_count - 1
	validate_tcc_macos_libgc_store_object(store.final_dylib, store.expected)!
	mut entries := os.ls(store.content_dir)!
	entries.sort()
	assert entries == [tcc_macos_libgc_name]
	for temporary in temporaries {
		assert !os.exists(temporary.path)
		assert !os.exists(temporary.directory)
	}
}
