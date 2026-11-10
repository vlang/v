module driver

import os
import v.flat
import v.gen.c as cgen
import v.pref

fn test_native_interface_contract_requires_known_paths_and_complete_manifest() ! {
	compiler := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.real_path(os.vtmp_dir()), 'native_interface_contract_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	headers := os.join_path(root, 'thirdparty', 'mbedtls', 'include', 'mbedtls')
	os.mkdir_all(headers)!
	header := os.real_path(os.join_path(headers, 'probe.h'))
	nested := os.real_path(os.join_path(headers, 'nested.h'))
	os.write_file(header, '#include "nested.h"\n')!
	os.write_file(nested, 'typedef int probe_value;\n')!
	mut a := flat.FlatAst.new()
	a.add_val(.file, os.join_path(root, 'vlib', 'net', 'mbedtls', 'mbedtls.c.v'))
	a.add_val(.module_decl, 'mbedtls')
	a.add_node(flat.Node{ kind: .directive, value: 'include', typ: flat.source_type_id('"${header}"') })
	inputs := cgen.CacheNativeInputs{
		module_inputs: {
			'mbedtls': [header]
		}
		native_paths:  {
			header: true
		}
	}
	prefs := pref.Preferences{ vroot: root, target: pref.host_target() }
	closure := v3_native_interface_closure(&a, &prefs, [], compiler, &inputs)
	assert closure.unassignable == ''
	assert header in closure.inputs['__v3_c_flags__']
	assert nested in closure.inputs['__v3_c_flags__']
	for input in closure.inputs['__v3_c_flags__'] {
		assert os.is_file(input), input
	}
	assert v3_native_interface_closure(&a, &prefs, [], compiler, &inputs).inputs == closure.inputs
	configuration_names := ['MBEDTLS_CONFIG_FILE', 'MBEDTLS_USER_CONFIG_FILE',
		'MBEDTLS_PSA_CRYPTO_CONFIG', 'MBEDTLS_PSA_CRYPTO_CONFIG_FILE',
		'MBEDTLS_PSA_CRYPTO_USER_CONFIG_FILE']
	for name in configuration_names {
		assert v3_native_interface_closure(&a, &prefs, ['-D${name}="custom.h"'], compiler, &inputs).unassignable != '', name
		for directive in ['define', 'undef'] {
			mut configured := flat.FlatAst.new()
			configured.add_node(flat.Node{ kind: .directive, value: directive, typ: flat.source_type_id('${name} "custom.h"') })
			assert v3_native_interface_closure(&configured, &prefs, [], compiler, &inputs).unassignable != '', '${directive} ${name}'
		}
		assert_native_interface_environment_override(name, &a, &prefs, compiler, &inputs)
	}
	for override in ['-Wp,-include,custom.h', '@custom.rsp'] {
		assert v3_native_interface_closure(&a, &prefs, [override], compiler, &inputs).unassignable != ''
	}
	mut first := V3ModuleCacheState{}
	assert prepare_v3_cache_external_inputs(mut first, &inputs, &closure)
	os.write_file(nested, 'typedef int other_value;\n')!
	changed := v3_native_interface_closure(&a, &prefs, [], compiler, &inputs)
	mut second := V3ModuleCacheState{}
	assert prepare_v3_cache_external_inputs(mut second, &inputs, &changed)
	assert first.external_input_digests[nested] != second.external_input_digests[nested]
	assert v3_native_interface_closure(&a, &prefs, [], 'tcc', &inputs).unassignable != ''
	for macro_name in ['__DATE__', '__TIME__', '__TIMESTAMP__'] {
		os.write_file(nested, 'static const char *probe_build = ${macro_name};\n')!
		assert v3_native_interface_closure(&a, &prefs, [], compiler, &inputs).unassignable != '', macro_name
		os.write_file(nested, '#define PROBE_DELAYED ${macro_name}\n')!
		assert v3_native_interface_closure(&a, &prefs, [], compiler, &inputs).unassignable != '', 'delayed ${macro_name}'
		// Forced headers precede the probe's poison pragma. A guard keeps the
		// later ordinary include from independently diagnosing the macro.
		os.write_file(nested, '#ifndef PROBE_FORCED_HEADER\n#define PROBE_FORCED_HEADER\nstatic const char *probe_build = ${macro_name};\n#endif\n')!
		for flags in [['-include', nested], ['-w', '-include', nested],
			['-Xclang', '-w', '-include', nested], ['-Xclang=-w', '-include', nested],
			['-Xclang', '-Wno-date-time', '-include', nested], ['--no-warnings', '-include', nested],
			['-Xclang', '--no-warnings', '-include', nested]] {
			assert v3_native_interface_closure(&a, &prefs, flags, compiler, &inputs).unassignable != '', '${macro_name}: ${flags}'
		}
	}
	os.write_file(nested, 'typedef int other_value;\n')!
	assert v3_native_interface_closure(&a, &prefs, ['-w', '-include', nested], compiler, &inputs).unassignable == ''
	os.rm(nested)!
	assert v3_native_interface_closure(&a, &prefs, [], compiler, &inputs).unassignable != ''
	assert !v3_web_native_interface(os.join_path(root, 'thirdparty', 'zstd', 'zstd.c'), root)
	assert !v3_web_native_interface(os.join_path(root, 'thirdparty', 'mbedtls', 'include_other', 'x.h'), root)
	assert !v3_web_native_interface(os.join_path(root, 'thirdparty', 'mbedtls', 'include', 'x.c'), root)
	forced := cgen.CacheNativeInputs{
		module_inputs: inputs.module_inputs
		macro_paths:   {
			header: true
		}
	}
	assert v3_native_interface_closure(&a, &prefs, [], compiler, &forced).unassignable != ''
}

fn assert_native_interface_environment_override(name string, a &flat.FlatAst, prefs &pref.Preferences, compiler string, inputs &cgen.CacheNativeInputs) {
	original := os.getenv_opt(name)
	defer {
		if value := original { os.setenv(name, value, true) } else { os.unsetenv(name) }
	}
	os.setenv(name, 'custom.h', true)
	assert v3_native_interface_closure(a, prefs, [], compiler, inputs).unassignable != '', name
}

fn test_native_interface_diagnostics_preserve_operands_when_removing_warning_suppression() {
	assert v3_native_interface_diagnostic_flags(['-w', '-include', '-w', '-I', '-w', '-D', '-w',
		'-Xclang', '-w', '-Xpreprocessor', '-w', '-Wno-date-time', '-Xclang=-w', '-Xclang',
		'-Wno-everything', '--no-warnings', '-Xclang', '--no-warnings', '-Xclang=--no-warnings']) == [
		'-include',
		'-w',
		'-I',
		'-w',
		'-D',
		'-w',
	]
}

fn test_tinycc_interface_contract_pin_rejects_bundle_edits() ! {
	assert v3_pinned_web_native_interfaces(@VEXEROOT)
	root := os.join_path(os.vtmp_dir(), 'pinned_interfaces_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	headers := os.join_path(root, 'thirdparty', 'mbedtls', 'include')
	os.mkdir_all(os.dir(headers))!
	os.cp_all(os.join_path(@VEXEROOT, 'thirdparty', 'mbedtls', 'include'), headers, false)!
	helper_dir := os.join_path(root, 'vlib', 'net', 'mbedtls')
	os.mkdir_all(helper_dir)!
	for name in ['mbedtls_helpers.h', 'mbedtls_threading.h'] {
		os.cp(os.join_path(@VEXEROOT, 'vlib', 'net', 'mbedtls', name), os.join_path(helper_dir, name))!
	}
	assert v3_pinned_web_native_interfaces(root)
	added := os.join_path(headers, '.added.inc')
	os.write_file(added, '#define LATER __DATE__\n')!
	assert !v3_pinned_web_native_interfaces(root)
	os.rm(added)!
	assert v3_pinned_web_native_interfaces(root)
	os.write_file(os.join_path(helper_dir, 'mbedtls_helpers.h'), 'modified')!
	assert !v3_pinned_web_native_interfaces(root)
	os.rm(os.join_path(helper_dir, 'mbedtls_helpers.h'))!
	assert !v3_pinned_web_native_interfaces(root)
}
