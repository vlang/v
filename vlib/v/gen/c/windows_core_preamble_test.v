module c

import os
import v.flat
import v.pref

fn windows_core_test_gen() FlatGen {
	mut g := FlatGen.new()
	g.a = &flat.FlatAst{}
	g.target = pref.target_from('windows', 'amd64') or { panic(err) }
	return g
}

fn test_windows_translation_unit_uses_headers_only_when_requested() {
	mut bare := windows_core_test_gen()
	bare.emit_translation_unit_include_directives()
	assert !bare.sb.str().contains('#include')
	mut headed := windows_core_test_gen()
	headed.add_c_directive('main', '#include <windows.h>', false)
	headed.emit_translation_unit_include_directives()
	assert headed.sb.str().count('#include <windows.h>') == 1
}

fn test_windows_core_records_match_native_sdk() {
	cc_name := $if windows { 'gcc' } $else { 'x86_64-w64-mingw32-gcc' }
	cc := os.find_abs_path_of_executable(cc_name) or { return }
	records := {
		'ULARGE_INTEGER':           ['LowPart', 'HighPart', 'QuadPart']
		'FILETIME':                 ['dwLowDateTime', 'dwHighDateTime']
		'SYSTEM_INFO':              ['dwOemId', 'wProcessorArchitecture', 'wReserved', 'dwPageSize',
			'lpMinimumApplicationAddress', 'lpMaximumApplicationAddress', 'dwActiveProcessorMask',
			'dwNumberOfProcessors', 'dwProcessorType', 'dwAllocationGranularity', 'wProcessorLevel',
			'wProcessorRevision']
		'MEMORYSTATUS':             ['dwLength', 'dwMemoryLoad', 'dwTotalPhys', 'dwAvailPhys',
			'dwTotalPageFile', 'dwAvailPageFile', 'dwTotalVirtual', 'dwAvailVirtual']
		'MEMORY_BASIC_INFORMATION': ['BaseAddress', 'AllocationBase', 'AllocationProtect', 'RegionSize',
			'State', 'Protect', 'Type']
		'PROCESS_MEMORY_COUNTERS':  ['cb', 'PageFaultCount', 'PeakWorkingSetSize', 'WorkingSetSize',
			'QuotaPeakPagedPoolUsage', 'QuotaPagedPoolUsage', 'QuotaPeakNonPagedPoolUsage',
			'QuotaNonPagedPoolUsage', 'PagefileUsage', 'PeakPagefileUsage']
	}
	mut lines := []string{}
	for name, _ in records {
		lines << '#define ${name} SDK_${name}\n#define _${name} SDK__${name}'
	}
	lines << '#include <windows.h>\n#include <psapi.h>\n#include <stdint.h>\n#include <stddef.h>'
	for name, _ in records {
		lines << '#undef ${name}\n#undef _${name}'
	}
	lines << 'typedef uint16_t u16; typedef uint32_t u32; typedef uint64_t u64;'
	mut g := windows_core_test_gen()
	g.headerless_windows_core_declarations()
	lines << g.sb.str()
	for name, fields in records {
		lines << '_Static_assert(sizeof(${name}) == sizeof(SDK_${name}), "${name} size");'
		lines << '_Static_assert(_Alignof(${name}) == _Alignof(SDK_${name}), "${name} alignment");'
		for field in fields {
			lines << '_Static_assert(offsetof(${name}, ${field}) == offsetof(SDK_${name}, ${field}), "${name}.${field} offset");'
			lines << '_Static_assert(sizeof(((${name}*)0)->${field}) == sizeof(((SDK_${name}*)0)->${field}), "${name}.${field} width");'
		}
	}
	lines << '#ifdef _WIN64\n_Static_assert(offsetof(MEMORY_BASIC_INFORMATION, PartitionId) == offsetof(SDK_MEMORY_BASIC_INFORMATION, PartitionId), "PartitionId offset");\n_Static_assert(sizeof(((MEMORY_BASIC_INFORMATION*)0)->PartitionId) == sizeof(((SDK_MEMORY_BASIC_INFORMATION*)0)->PartitionId), "PartitionId width");\n#endif'
	dir := os.join_path(os.vtmp_dir(), 'windows_core_sdk_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'sdk.c')
	os.write_file(source, lines.join('\n'))!
	result := os.execute('${os.quoted_path(cc)} -std=gnu11 -fsyntax-only ${os.quoted_path(source)}')
	assert result.exit_code == 0, result.output
}

fn test_windows_native_callback_thunks_match_sdk_calling_conventions() {
	cc_name := $if windows { 'gcc' } $else { 'x86_64-w64-mingw32-gcc' }
	cc := os.find_abs_path_of_executable(cc_name) or { return }
	dir := os.join_path(os.vtmp_dir(), 'windows_core_thunks_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	for arch_flags in ['', '-m32'] {
		for name in ['vlib/builtin/core_windows.c', 'vlib/sync/thread_helper.c'] {
			source := os.join_path(@VEXEROOT, name)
			object := os.join_path(dir, 'helper.o')
			result := os.execute('${os.quoted_path(cc)} ${arch_flags} -std=gnu11 -Werror=implicit-function-declaration -Werror=incompatible-pointer-types -c ${os.quoted_path(source)} -o ${os.quoted_path(object)}')
			assert result.exit_code == 0, result.output
		}
	}
}

fn test_windows_sdk_owns_metadata_handle_declarations() {
	for arch in ['x86', 'amd64'] {
		mut g := windows_core_test_gen()
		g.set_target(pref.target_from('windows', arch) or { panic(err) })
		assert g.c_extern_calling_convention('GetFileInformationByHandle') == 'WINAPI'
		assert g.should_emit_c_extern_decl('GetFileInformationByHandle')
		g.add_c_directive('main', '#include <windows.h>', false)
		assert !g.should_emit_c_extern_decl('GetFileInformationByHandle')
	}
}

fn test_windows_error_macros_match_sdk() {
	cc_name := $if windows { 'gcc' } $else { 'x86_64-w64-mingw32-gcc' }
	cc := os.find_abs_path_of_executable(cc_name) or { return }
	mut g := windows_core_test_gen()
	g.headerless_windows_core_declarations()
	prelude := g.sb.str()
	assert prelude.contains('#define ERROR_INSUFFICIENT_BUFFER 122'), 'vlib/os uses C.ERROR_INSUFFICIENT_BUFFER, so the prelude must define it'
	// carry only the ERROR_ defines into the SDK check: the prelude structs
	// intentionally collide with windows.h by design (that is why V is
	// headerless), so only the macro values are compared
	mut defines := []string{}
	for line in prelude.split_into_lines() {
		if line.starts_with('#define ERROR_') {
			defines << line.replace('ERROR_', 'SDK_ERROR_')
		}
	}
	assert defines.len > 0, 'prelude must define at least one ERROR_ macro'
	lines := defines.join('\n') + '\n#include <windows.h>\n_Static_assert(SDK_ERROR_INSUFFICIENT_BUFFER == ERROR_INSUFFICIENT_BUFFER, "ERROR_INSUFFICIENT_BUFFER value");\n'
	dir := os.join_path(os.vtmp_dir(), 'windows_core_errmacros_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'errmacros.c')
	os.write_file(source, lines)!
	result := os.execute('${os.quoted_path(cc)} -std=gnu11 -fsyntax-only ${os.quoted_path(source)}')
	assert result.exit_code == 0, result.output
}
