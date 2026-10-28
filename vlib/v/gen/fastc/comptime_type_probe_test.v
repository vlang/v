module fastc

import v.flat
import v.pref
import v.scanner
import v.token

fn test_qualified_comptime_iteration_does_not_retain_generated_c_name_inputs() {
	source := r'$for field in dep.FastcComptimeMissingProbe.fields {}'
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('comptime_probe.v', source.len), source)
	first := scan.scan()
	mut g := Parser{
		prefs:   prefs
		s:       scan
		tok:     first
		lit:     scan.lit
		imports: {
			'dep': 'comptime_probe_module'
		}
	}
	name := 'comptime_probe_module.FastcComptimeMissingProbe'
	assert flat.source_type_existing_id(name) == none
	assert !g.parse_comptime_for_statement()!
	assert flat.source_type_existing_id(name) == none
}
