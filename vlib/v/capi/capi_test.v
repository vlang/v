module capi

import os

fn fixture_dir() string {
	return os.join_path(@VEXEROOT, 'vlib', 'v', 'capi', 'testdata', 'mockpkg')
}

fn test_sanitize() {
	assert sanitize('metif12.vtls/tls') == 'metif12_vtls_tls'
	assert sanitize('Conn.read') == 'Conn_read'
}

fn test_collect_model() {
	m := collect_model(fixture_dir(), '') or {
		assert false, 'collect_model failed: ${err.msg()}'
		return
	}
	assert m.module_name == 'mockpkg'
	assert m.prefix == 'mockpkg'
	assert m.funcs.len == 2
	assert m.types.len == 2
	assert m.consts.len == 1
	assert m.funcs[0].c_name == 'mockpkg_add'
	assert m.funcs[0].doc == 'add returns the sum of a and b.'
	assert m.is_struct('Client')
	assert m.is_enum('Mode')
}

fn test_qualified_wrappers_for_combined_build() {
	dir := os.join_path(@VEXEROOT, 'vlib', 'v', 'capi', 'testdata', 'mockpkg')
	m := collect_model_qualified(dir, 'lib_mock', 'mock') or {
		assert false, 'collect_model_qualified failed: ${err.msg()}'
		return
	}
	mut warnings := []string{}
	code, infos := generate_wrappers(m, mut warnings)
	assert code.starts_with('module main')
	assert code.contains('mock.add(p0, p1)')
	assert code.contains('&mock.Client(self)')
	assert infos[0].c_name == 'lib_mock_add'
}

fn test_manual_export_overrides() {
	dir := os.join_path(@VEXEROOT, 'vlib', 'v', 'capi', 'testdata', 'attrs')
	m := collect_model(dir, '') or {
		assert false, 'collect_model failed: ${err.msg()}'
		return
	}
	assert m.funcs.len == 2
	mut names := []string{}
	for f in m.funcs {
		names << f.c_name
	}
	assert 'renamed_sum' in names, names.join(', ')
	assert 'attrprobe_internal' in names, names.join(', ')
	assert 'attrprobe_hidden' !in names, names.join(', ')
}

fn test_generate_wrappers() {
	m := collect_model(fixture_dir(), '') or {
		assert false, 'collect_model failed: ${err.msg()}'
		return
	}
	mut warnings := []string{}
	code, infos := generate_wrappers(m, mut warnings)
	assert warnings.len == 0, warnings.join('\n')
	assert code.contains("@[export: 'mockpkg_add']")
	assert code.contains('fn capi_export_0(p0 int, p1 int) int {')
	assert code.contains("@[export: 'mockpkg_Client_read']")
	assert code.contains('fn capi_export_1(self voidptr, p1 &u8, p1_len int, out &int) int {')
	assert code.contains("@[export: 'mockpkg_get_version']")
	assert infos.len == 3
	assert infos[0].signature == 'int mockpkg_add(int p0, int p1)'
}

fn test_emit_artifacts() {
	m := collect_model(fixture_dir(), '') or {
		assert false, 'collect_model failed: ${err.msg()}'
		return
	}
	mut warnings := []string{}
	_, infos := generate_wrappers(m, mut warnings)
	dir := os.join_path(os.vtmp_dir(), 'v-capi-emit-test')
	os.mkdir_all(dir) or {}
	res := emit_files(m, infos, EmitOptions{
		out_dir:  dir
		lib_name: 'mockpkg'
		version:  '1.2.3'
	}) or {
		assert false, 'emit_files failed: ${err.msg()}'
		return
	}
	_ = res
	h := os.read_file(os.join_path(dir, 'mockpkg.h')) or {
		assert false, 'header missing'
		return
	}
	assert h.contains('#define MOCKPKG_VERSION "1.2.3"')
	assert h.contains('int mockpkg_add(int p0, int p1);')
	assert h.contains('typedef struct mockpkg_Client mockpkg_Client;')
	d := os.read_file(os.join_path(dir, 'mockpkg.def')) or {
		assert false, 'def missing'
		return
	}
	assert d.contains('\tmockpkg_add')
	p := os.read_file(os.join_path(dir, 'mockpkg.pc')) or {
		assert false, 'pc missing'
		return
	}
	assert p.contains('Version: 1.2.3')
	assert p.contains('-lmockpkg')
}
