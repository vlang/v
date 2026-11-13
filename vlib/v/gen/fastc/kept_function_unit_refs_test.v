module fastc

import os
import v.pref

fn test_fastc_unit_prototypes_ignore_refs_in_pruned_same_named_function() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.no_parallel = true
	// The early source-name pass keeps both selected functions. Only the
	// remote one is reachable from main, so the body-level prune removes the
	// spare one. Its target call must not pull a remote prototype into the
	// spare file's translation unit.
	texts := [
		r'module mathspare
import mathremote
pub fn live_here() int { return 13 }
fn selected() int { return mathremote.target() }
',
		r'module mathremote
pub fn selected() int { return target() }
pub fn target() int { return 29 }
',
		r'module main
import mathspare
import mathremote
fn main() { assert mathspare.live_here() + mathremote.selected() == 42 }
',
	]
	mut sources := []FastcSourceFile{}
	for i, text in texts {
		path := os.join_path(os.vtmp_dir(), 'kept_function_unit_refs_${i}.v')
		sources << FastcSourceFile{
			path:   path
			source: text
			header: fastc_scan_source_header(text, path, prefs)!
		}
	}
	mut units := FastcUnitLayout{}
	mut native_symbols := []string{}
	mut native_declarations := map[string]string{}
	pieces, _, _ := generate_source_pieces(sources, map[string]string{}, prefs, [], mut units, mut native_symbols, mut native_declarations)!
	assert units.unit_ref_starts.len == units.unit_starts.len
	assert units.unit_starts.len >= 4
	monolithic := fastc_join_c_pieces(pieces)
	assert monolithic.contains('mathremote__target(void);')
	assert monolithic.contains('mathremote__target(void) {')
	assert !monolithic.contains('mathspare__selected(void) {')
	mut found_spare := false
	mut found_remote := false
	for u in 0 .. units.unit_starts.len - 1 {
		body := fastc_join_c_pieces(pieces[units.unit_starts[u]..units.unit_starts[u + 1]])
		if body.contains('mathspare__live_here(void) {') {
			found_spare = true
			rendered := fastc_render_c_unit(pieces, &units, if u == 0 { 0 } else { 1 }, u, u + 1)
			assert rendered.contains('mathspare__live_here(void);')
			assert !rendered.contains('mathremote__target(void);'), rendered
			assert !rendered.contains('mathspare__selected(void);'), rendered
		}
		if body.contains('mathremote__target(void) {') {
			found_remote = true
			rendered := fastc_render_c_unit(pieces, &units, if u == 0 { 0 } else { 1 }, u, u + 1)
			assert rendered.contains('mathremote__target(void);'), rendered
			assert rendered.contains('mathremote__selected(void);'), rendered
		}
	}
	assert found_spare
	assert found_remote
}
