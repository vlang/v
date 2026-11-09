module parser

import v.pref

fn test_cached_header_preserves_export_metadata_and_attributes() {
	for module_name in ['exportprobe', 'builtin', 'main'] {
		mut p := Parser.new(pref.new_preferences())
		a := p.parse_text('probe.vh', "module ${module_name}
@[export: 'native_callback'; weak]
pub fn callback() int
fn ordinary() int
")
		assert p.diagnostics.len == 0, p.diagnostics.str()
		qualified_name := if module_name == 'exportprobe' {
			'exportprobe.callback'
		} else {
			'callback'
		}
		assert a.export_fn_names == {
			qualified_name: 'native_callback'
		}
		assert p.export_records == [ExportRecord{
			name:  'callback'
			qname: qualified_name
			value: 'native_callback'
		}]
		functions := a.nodes.filter(it.kind == .fn_decl)
		assert functions.len == 2
		assert functions.all(it.is_mut)
		attributes := a.nodes.filter(it.kind == .directive && it.value.starts_with('@attributes:'))
		assert attributes.len == 1
		assert attributes[0].generic_params() == ["export: 'native_callback'", 'weak']
	}
}

fn test_cached_header_does_not_register_disabled_exports() {
	mut p := Parser.new(pref.new_preferences())
	a := p.parse_text('probe.vh', "module exportprobe
@[if false; export: 'disabled_callback']
pub fn disabled() int
fn ordinary() int
")
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert 'exportprobe.disabled' in a.disabled_fns
	assert a.export_fn_names.len == 0
	assert p.export_records.len == 0
}
