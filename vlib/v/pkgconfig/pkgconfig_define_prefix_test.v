module main

import os
import v.pkgconfig

const synthetic_prefix = '/synthetic-sdk'

struct PrefixRelocationFixture {
	base_dir   string
	prefix_dir string
	pc_dir     string
}

fn normalized_prefix_path(path string) string {
	return os.real_path(path).replace('\\', '/').trim_right('/')
}

fn normalized_prefix_flags(flags []string) []string {
	return flags.map(it.replace('\\', '/'))
}

fn relocated_path(prefix string, suffix string) string {
	return '${prefix}/${suffix}'
}

fn write_prefix_fixture(pc_dir string, name string, content string) {
	os.write_file(os.join_path(pc_dir, '${name}.pc'), content) or { panic(err) }
}

fn new_prefix_relocation_fixture(label string) PrefixRelocationFixture {
	base_dir := os.join_path(os.vtmp_dir(), 'pkgconfig relocation ${os.getpid()} ${label}')
	prefix_dir := os.join_path(base_dir, 'non standard sdk root')
	pc_dir := os.join_path(prefix_dir, 'lib', 'pkgconfig')
	os.rmdir_all(base_dir) or {}
	os.mkdir_all(pc_dir) or { panic(err) }

	write_prefix_fixture(pc_dir, 'relocatable-direct', r'prefix=/synthetic-sdk
includedir=${prefix}/include
libdir=${prefix}/lib
absoluteincludedir=/synthetic-sdk/absolute/include
absolutelibdir=/synthetic-sdk/absolute/lib
neighborincludedir=/synthetic-sdk-neighbor/include
neighborlibdir=/synthetic-sdk-neighbor/lib
lookalikeincludedir=/synthetic-sdk2/include
lookalikelibdir=/synthetic-sdk2/lib

Name: relocatable-direct
Description: Direct relocatable metadata
Version: 1.0.0
Cflags: -I${includedir}/direct -I${absoluteincludedir} -I${neighborincludedir} -I${lookalikeincludedir} -DRELOCATABLE_DIRECT
Cflags.private: -I${prefix}/private/direct -DRELOCATABLE_DIRECT_PRIVATE
Libs: -L${libdir}/direct -L${absolutelibdir} -L${neighborlibdir} -L${lookalikelibdir} -lrelocatable_direct
Libs.private: -L${prefix}/private/lib -lrelocatable_direct_private
')
	write_prefix_fixture(pc_dir, 'relocatable-root', r'prefix=/synthetic-sdk
includedir=${prefix}/include
libdir=${prefix}/lib

Name: relocatable-root
Description: Relocatable dependency root
Version: 1.0.0
Requires: relocatable-public
Requires.private: relocatable-private
Cflags: -I${includedir}/root -DRELOCATABLE_ROOT
Cflags.private: -I${prefix}/private/root -DRELOCATABLE_ROOT_PRIVATE
Libs: -L${libdir}/root -lrelocatable_root
Libs.private: -L${prefix}/private/lib/root -lrelocatable_root_private
')
	write_prefix_fixture(pc_dir, 'relocatable-public', r'prefix=/synthetic-sdk
includedir=${prefix}/include
libdir=${prefix}/lib

Name: relocatable-public
Description: Public relocatable dependency
Version: 1.0.0
Cflags: -I${includedir}/public -DRELOCATABLE_PUBLIC
Cflags.private: -I${prefix}/private/public -DRELOCATABLE_PUBLIC_PRIVATE
Libs: -L${libdir}/public -lrelocatable_public
Libs.private: -L${prefix}/private/lib/public -lrelocatable_public_private
')
	write_prefix_fixture(pc_dir, 'relocatable-private', r'prefix=/synthetic-sdk
includedir=${prefix}/include
libdir=${prefix}/lib

Name: relocatable-private
Description: Private relocatable dependency
Version: 1.0.0
Cflags: -I${includedir}/private -DRELOCATABLE_PRIVATE
Cflags.private: -I${prefix}/private/private -DRELOCATABLE_PRIVATE_PRIVATE
Libs: -L${libdir}/private -lrelocatable_private
Libs.private: -L${prefix}/private/lib/private -lrelocatable_private_private
')

	return PrefixRelocationFixture{
		base_dir:   base_dir
		prefix_dir: prefix_dir
		pc_dir:     pc_dir
	}
}

fn new_single_prefix_fixture(label string, metadata_parent string, name string, content string) PrefixRelocationFixture {
	base_dir := os.join_path(os.vtmp_dir(), 'pkgconfig relocation ${os.getpid()} ${label}')
	prefix_dir := os.join_path(base_dir, 'non standard sdk root')
	pc_dir := os.join_path(prefix_dir, metadata_parent, 'pkgconfig')
	os.rmdir_all(base_dir) or {}
	os.mkdir_all(pc_dir) or { panic(err) }
	write_prefix_fixture(pc_dir, name, content)
	return PrefixRelocationFixture{
		base_dir:   base_dir
		prefix_dir: prefix_dir
		pc_dir:     pc_dir
	}
}

fn new_nonstandard_metadata_fixture(label string, name string, content string) PrefixRelocationFixture {
	base_dir := os.join_path(os.vtmp_dir(), 'pkgconfig relocation ${os.getpid()} ${label}')
	prefix_dir := os.join_path(base_dir, 'non standard sdk root')
	pc_dir := os.join_path(prefix_dir, 'lib', 'metadata')
	os.rmdir_all(base_dir) or {}
	os.mkdir_all(pc_dir) or { panic(err) }
	write_prefix_fixture(pc_dir, name, content)
	return PrefixRelocationFixture{
		base_dir:   base_dir
		prefix_dir: prefix_dir
		pc_dir:     pc_dir
	}
}

fn resolve_prefix_fixture(fixture PrefixRelocationFixture, packages []string, mode pkgconfig.LinkMode) pkgconfig.ResolvedFlags {
	return pkgconfig.resolve(packages, pkgconfig.Options{
		path:              fixture.pc_dir
		use_default_paths: false
		link_mode:         mode
	}) or { panic(err) }
}

fn test_pkgconfig_relocates_direct_metadata_in_dynamic_mode() {
	$if !windows {
		return
	}
	fixture := new_prefix_relocation_fixture('dynamic direct')
	defer {
		os.rmdir_all(fixture.base_dir) or {}
	}
	prefix := normalized_prefix_path(fixture.prefix_dir)
	flags := resolve_prefix_fixture(fixture, ['relocatable-direct'], .dynamic)

	assert normalized_prefix_flags(flags.cflags) == [
		'-I${relocated_path(prefix, 'include/direct')}',
		'-I${relocated_path(prefix, 'absolute/include')}',
		'-I${synthetic_prefix}-neighbor/include',
		'-I${synthetic_prefix}2/include',
		'-DRELOCATABLE_DIRECT',
	]
	assert normalized_prefix_flags(flags.libs) == [
		'-L${relocated_path(prefix, 'lib/direct')}',
		'-L${relocated_path(prefix, 'absolute/lib')}',
		'-L${synthetic_prefix}-neighbor/lib',
		'-L${synthetic_prefix}2/lib',
		'-lrelocatable_direct',
	]
}

fn test_pkgconfig_does_not_relocate_metadata_outside_pkgconfig_directory() {
	$if !windows {
		return
	}
	fixture := new_nonstandard_metadata_fixture('non metadata layout', 'nonrelocatable-layout', r'prefix=/synthetic-sdk
includedir=${prefix}/include
libdir=${prefix}/lib

Name: nonrelocatable-layout
Description: Metadata outside a pkgconfig directory
Version: 1.0.0
Cflags: -I${includedir}/layout
Libs: -L${libdir}/layout -lnonrelocatable_layout
')
	defer {
		os.rmdir_all(fixture.base_dir) or {}
	}
	for mode in [pkgconfig.LinkMode.dynamic, .static_] {
		flags := resolve_prefix_fixture(fixture, ['nonrelocatable-layout'], mode)
		assert normalized_prefix_flags(flags.cflags) == [
			'-I${synthetic_prefix}/include/layout',
		]
		assert normalized_prefix_flags(flags.libs) == [
			'-L${synthetic_prefix}/lib/layout',
			'-lnonrelocatable_layout',
		]
	}
}

fn test_pkgconfig_relocates_windows_paths_case_insensitively() {
	$if !windows {
		return
	}
	fixture := new_single_prefix_fixture('mixed case windows path', 'lib', 'relocatable-case', r'prefix=/Synthetic-SDK
includedir=/synthetic-sdk\include
libdir=/SYNTHETIC-sdk\lib

Name: relocatable-case
Description: Mixed-case Windows path metadata
Version: 1.0.0
Cflags: -I${includedir}/case
Libs: -L${libdir}/case -lrelocatable_case
')
	defer {
		os.rmdir_all(fixture.base_dir) or {}
	}
	prefix := normalized_prefix_path(fixture.prefix_dir)
	for mode in [pkgconfig.LinkMode.dynamic, .static_] {
		flags := resolve_prefix_fixture(fixture, ['relocatable-case'], mode)
		assert normalized_prefix_flags(flags.cflags) == [
			'-I${relocated_path(prefix, 'include/case')}',
		]
		assert normalized_prefix_flags(flags.libs) == [
			'-L${relocated_path(prefix, 'lib/case')}',
			'-lrelocatable_case',
		]
	}
}

fn test_pkgconfig_relocates_variables_declared_before_and_after_prefix() {
	$if !windows {
		return
	}
	fixture := new_single_prefix_fixture('variable order', 'lib', 'relocatable-order', r'before=/synthetic-sdk/before
prefix=/synthetic-sdk
after=/synthetic-sdk/after

Name: relocatable-order
Description: Variable ordering metadata
Version: 1.0.0
Cflags: -I${before}/include -I${after}/include
Libs: -L${before}/lib -L${after}/lib -lrelocatable_order
')
	defer {
		os.rmdir_all(fixture.base_dir) or {}
	}
	prefix := normalized_prefix_path(fixture.prefix_dir)
	for mode in [pkgconfig.LinkMode.dynamic, .static_] {
		flags := resolve_prefix_fixture(fixture, ['relocatable-order'], mode)
		assert normalized_prefix_flags(flags.cflags) == [
			'-I${synthetic_prefix}/before/include',
			'-I${relocated_path(prefix, 'after/include')}',
		]
		assert normalized_prefix_flags(flags.libs) == [
			'-L${synthetic_prefix}/before/lib',
			'-L${relocated_path(prefix, 'after/lib')}',
			'-lrelocatable_order',
		]
	}
}

fn test_pkgconfig_relocates_share_pkgconfig_layout() {
	$if !windows {
		return
	}
	fixture := new_single_prefix_fixture('share layout', 'share', 'relocatable-share', r'prefix=/synthetic-sdk
includedir=${prefix}/include
libdir=${prefix}/lib

Name: relocatable-share
Description: Shared metadata layout
Version: 1.0.0
Cflags: -I${includedir}/share
Libs: -L${libdir}/share -lrelocatable_share
')
	defer {
		os.rmdir_all(fixture.base_dir) or {}
	}
	prefix := normalized_prefix_path(fixture.prefix_dir)
	for mode in [pkgconfig.LinkMode.dynamic, .static_] {
		flags := resolve_prefix_fixture(fixture, ['relocatable-share'], mode)
		assert normalized_prefix_flags(flags.cflags) == [
			'-I${relocated_path(prefix, 'include/share')}',
		]
		assert normalized_prefix_flags(flags.libs) == [
			'-L${relocated_path(prefix, 'lib/share')}',
			'-lrelocatable_share',
		]
	}
}

fn test_pkgconfig_preserves_drive_absolute_and_unc_prefixes() {
	$if !windows {
		return
	}
	drive_fixture := new_single_prefix_fixture('drive absolute', 'lib', 'native-drive-prefix', r'prefix=C:/already/absolute
includedir=${prefix}/include
libdir=${prefix}/lib

Name: native-drive-prefix
Description: Native drive-absolute metadata
Version: 1.0.0
Cflags: -I${includedir}
Libs: -L${libdir} -lnative_drive_prefix
')
	defer {
		os.rmdir_all(drive_fixture.base_dir) or {}
	}
	for mode in [pkgconfig.LinkMode.dynamic, .static_] {
		flags := resolve_prefix_fixture(drive_fixture, ['native-drive-prefix'], mode)
		assert normalized_prefix_flags(flags.cflags) == [
			'-IC:/already/absolute/include',
		]
		assert normalized_prefix_flags(flags.libs) == [
			'-LC:/already/absolute/lib',
			'-lnative_drive_prefix',
		]
	}

	unc_fixture := new_single_prefix_fixture('unc absolute', 'share', 'native-unc-prefix', r'prefix=//server/share/sdk
includedir=${prefix}/include
libdir=${prefix}/lib

Name: native-unc-prefix
Description: Native UNC metadata
Version: 1.0.0
Cflags: -I${includedir}
Libs: -L${libdir} -lnative_unc_prefix
')
	defer {
		os.rmdir_all(unc_fixture.base_dir) or {}
	}
	for mode in [pkgconfig.LinkMode.dynamic, .static_] {
		flags := resolve_prefix_fixture(unc_fixture, ['native-unc-prefix'], mode)
		assert normalized_prefix_flags(flags.cflags) == [
			'-I//server/share/sdk/include',
		]
		assert normalized_prefix_flags(flags.libs) == [
			'-L//server/share/sdk/lib',
			'-lnative_unc_prefix',
		]
	}
}

fn test_pkgconfig_relocates_dependency_closure_in_dynamic_mode() {
	$if !windows {
		return
	}
	fixture := new_prefix_relocation_fixture('dynamic closure')
	defer {
		os.rmdir_all(fixture.base_dir) or {}
	}
	prefix := normalized_prefix_path(fixture.prefix_dir)
	flags := resolve_prefix_fixture(fixture, ['relocatable-root'], .dynamic)

	assert normalized_prefix_flags(flags.cflags) == [
		'-I${relocated_path(prefix, 'include/root')}',
		'-DRELOCATABLE_ROOT',
		'-I${relocated_path(prefix, 'include/public')}',
		'-DRELOCATABLE_PUBLIC',
		'-I${relocated_path(prefix, 'include/private')}',
		'-DRELOCATABLE_PRIVATE',
	]
	assert normalized_prefix_flags(flags.libs) == [
		'-L${relocated_path(prefix, 'lib/root')}',
		'-lrelocatable_root',
		'-L${relocated_path(prefix, 'lib/public')}',
		'-lrelocatable_public',
		'-L${relocated_path(prefix, 'lib/private')}',
		'-lrelocatable_private',
	]
}

fn test_pkgconfig_relocates_direct_private_metadata_in_static_mode() {
	$if !windows {
		return
	}
	fixture := new_prefix_relocation_fixture('static direct')
	defer {
		os.rmdir_all(fixture.base_dir) or {}
	}
	prefix := normalized_prefix_path(fixture.prefix_dir)
	flags := resolve_prefix_fixture(fixture, ['relocatable-direct'], .static_)

	assert normalized_prefix_flags(flags.cflags) == [
		'-I${relocated_path(prefix, 'include/direct')}',
		'-I${relocated_path(prefix, 'absolute/include')}',
		'-I${synthetic_prefix}-neighbor/include',
		'-I${synthetic_prefix}2/include',
		'-DRELOCATABLE_DIRECT',
		'-I${relocated_path(prefix, 'private/direct')}',
		'-DRELOCATABLE_DIRECT_PRIVATE',
	]
	assert normalized_prefix_flags(flags.libs) == [
		'-L${relocated_path(prefix, 'lib/direct')}',
		'-L${relocated_path(prefix, 'absolute/lib')}',
		'-L${synthetic_prefix}-neighbor/lib',
		'-L${synthetic_prefix}2/lib',
		'-lrelocatable_direct',
		'-L${relocated_path(prefix, 'private/lib')}',
		'-lrelocatable_direct_private',
	]
}

fn test_pkgconfig_relocates_public_and_private_dependencies_in_static_mode() {
	$if !windows {
		return
	}
	fixture := new_prefix_relocation_fixture('static closure')
	defer {
		os.rmdir_all(fixture.base_dir) or {}
	}
	prefix := normalized_prefix_path(fixture.prefix_dir)
	flags := resolve_prefix_fixture(fixture, ['relocatable-root'], .static_)

	assert normalized_prefix_flags(flags.cflags) == [
		'-I${relocated_path(prefix, 'include/root')}',
		'-DRELOCATABLE_ROOT',
		'-I${relocated_path(prefix, 'include/public')}',
		'-DRELOCATABLE_PUBLIC',
		'-I${relocated_path(prefix, 'include/private')}',
		'-DRELOCATABLE_PRIVATE',
		'-I${relocated_path(prefix, 'private/root')}',
		'-DRELOCATABLE_ROOT_PRIVATE',
		'-I${relocated_path(prefix, 'private/public')}',
		'-DRELOCATABLE_PUBLIC_PRIVATE',
		'-I${relocated_path(prefix, 'private/private')}',
		'-DRELOCATABLE_PRIVATE_PRIVATE',
	]
	assert normalized_prefix_flags(flags.libs) == [
		'-L${relocated_path(prefix, 'lib/root')}',
		'-lrelocatable_root',
		'-L${relocated_path(prefix, 'private/lib/root')}',
		'-lrelocatable_root_private',
		'-L${relocated_path(prefix, 'lib/public')}',
		'-lrelocatable_public',
		'-L${relocated_path(prefix, 'private/lib/public')}',
		'-lrelocatable_public_private',
		'-L${relocated_path(prefix, 'lib/private')}',
		'-lrelocatable_private',
		'-L${relocated_path(prefix, 'private/lib/private')}',
		'-lrelocatable_private_private',
	]
}
