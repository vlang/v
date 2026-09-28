module driver

import v.flat
import v.pref

fn test_linux_shared_build_skips_implicit_tcc() {
	linux_target := pref.Target{
		os:   'linux'
		arch: 'amd64'
	}
	base := V3BundledTccProbeOptions{
		backend:     'c'
		c_compiler:  'cc'
		host_os:     'linux'
		host_target: linux_target
		target:      linux_target
		bundled_tcc: '/tmp/tcc.exe'
		is_shared:   true
	}
	assert !v3_should_probe_bundled_tcc(base)
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		is_liveshared: true
	})
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		c_compiler:          'tcc'
		c_compiler_explicit: true
	})
}

fn test_shared_exports_version_script_hides_unlisted_symbols() {
	script := v3_shared_exports_version_script({
		'mylib.compute':        'mylib_compute'
		'mylib.private_export': 'mylib_private_export'
	}, ['mylib_counter'])
	assert script.contains('"mylib_compute";')
	assert script.contains('"mylib_counter";')
	assert script.contains('"mylib_private_export";')
	assert script.contains('local: *;')
	assert !script.contains('mylib.compute')
	assert v3_shared_exports_version_script(map[string]string{}, []).contains('local: *;')
}

fn test_shared_exports_collects_global_abi_names() {
	mut ast := flat.FlatAst.new()
	field_id := ast.add_node(flat.Node{ kind: .ident, value: 'counter' })
	start := ast.children.len
	ast.children << field_id
	global_id := ast.add_node(flat.Node{
		kind:           .global_decl
		children_start: start
		children_count: flat.child_count(1)
	})
	mut attr := flat.Node{ kind: .directive, value: '@attributes:${int(global_id)}' }
	attr.set_generic_params(["export: 'mylib_counter'"])
	ast.add_node(attr)
	assert v3_exported_global_names(&ast) == ['mylib_counter']
}
