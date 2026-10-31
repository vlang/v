module fastc

import os
import v.pref

fn fastc_native_pruning_fixture(sources []string) ![]string {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	mut files := []FastcSourceFile{}
	for i, source in sources {
		path := os.join_path(os.vtmp_dir(), 'fastc_native_pruning_${i}.v')
		files << FastcSourceFile{
			path:   path
			source: source
			header: fastc_scan_source_header(source, path, prefs)!
		}
	}
	_, _, flags := generate_source_files(files, map[string]string{}, prefs)!
	return flags.filter(fastc_flag_is_native_input(it)).map(os.file_name(it))
}

fn test_fastc_native_inputs_keep_live_bodies_and_unconditional_artifacts() {
	flags := fastc_native_pruning_fixture([
		r'module live
#flag @DIR/retained_by_body.c
fn C.unused_native_declaration() int
pub fn answer() int { return 42 }
',
		r'module unknown
#flag @DIR/no_native_declarations.c
pub struct Marker {}
',
		r'module artifacts
#flag @DIR/constructor.a
#flag @DIR/registration.so.2
fn C.unused_binary_declaration() int
pub struct Marker {}
',
		r'module main
import live
import unknown
import artifacts
fn main() {
	assert live.answer() == 42
	_ := unknown.Marker{}
	_ := artifacts.Marker{}
}
',
	]) or { panic(err) }
	assert flags == ['retained_by_body.c', 'no_native_declarations.c', 'constructor.a',
		'registration.so.2'], flags.str()
}

fn test_fastc_native_inputs_scan_unresolved_owners_and_preserve_flag_order() {
	flags := fastc_native_pruning_fixture([
		r'module unused
#flag @DIR/dropped_unused.c
#flag @DIR/shared_native.c
fn C.unused_native_input() int
pub struct Marker {}
',
		r'module foreign
#flag @DIR/retained_by_foreign_call.c
fn C.cross_module_native_input() int
pub struct Marker {}
',
		r'module blank
#flag @DIR/retained_by_blank_import.c
fn C.blank_native_input() int
',
		r'module initialized
#flag @DIR/retained_by_initializer.c
fn C.initializer_native_input() int
fn init() { _ := 42 }
',
		r'module shared
#flag @DIR/shared_native.c
fn C.shared_native_input() int
pub fn answer() int { return 42 }
',
		r'module main
import unused
import foreign
import blank as _
import initialized
import shared
fn main() {
	_ := unused.Marker{}
	_ := foreign.Marker{}
	assert C.cross_module_native_input() == shared.answer()
}
',
	]) or { panic(err) }
	assert flags == ['shared_native.c', 'retained_by_foreign_call.c', 'retained_by_blank_import.c',
		'retained_by_initializer.c', 'shared_native.c'], flags.str()
}
