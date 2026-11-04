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
	for override in ['-DMBEDTLS_CONFIG_FILE="custom.h"', '-DMBEDTLS_USER_CONFIG_FILE="custom.h"',
		'-Wp,-include,custom.h', '@custom.rsp'] {
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
	}
	os.write_file(nested, 'typedef int other_value;\n')!
	mut configured := flat.FlatAst.new()
	configured.add_node(flat.Node{ kind: .directive, value: 'define', typ: flat.source_type_id('MBEDTLS_USER_CONFIG_FILE "custom.h"') })
	assert v3_native_interface_closure(&configured, &prefs, [], compiler, &inputs).unassignable != ''
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
