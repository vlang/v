module transform

import os
import v.flat
import v.parser
import v.pref
import v.types

fn embedded_receiver_scope_fixture(tag string) !string {
	root := os.join_path(os.vtmp_dir(), 'embedded_receiver_scope_${tag}_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'appkit'))!
	os.mkdir_all(os.join_path(root, 'model'))!
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'embedded_receiver_scope' }")!
	os.write_file(os.join_path(root, 'model', 'model.v'), 'module model
pub struct Base { pub: value int }
pub fn (b Base) read() int { return b.value }
pub struct Selected { pub: value int }
pub fn (s Selected) read() int { return s.value }
')!
	os.write_file(os.join_path(root, 'appkit', 'appkit.v'), 'module appkit
import model as imported
pub struct App { pub: value int }
pub fn (a App) read() int { return a.value }
pub struct Layer { App }
pub struct Imported { imported.Base }
pub type Callback = fn () int
pub fn (f Callback) read() int { return f() }
pub struct CallbackLayer { Callback }
pub struct Holder[T] { pub: value T }
pub fn (h Holder[T]) read() T { return h.value }
pub struct Generic[T] { Holder[T] }
')!
	os.write_file(os.join_path(root, 'appkit', 'selected.v'), 'module appkit
import model { Selected }
pub struct Selection { Selected }
')!
	os.write_file(os.join_path(root, 'main.v'), 'module main
import appkit
import model
struct App { appkit.Layer }
fn (a App) local_only() int { return 999 }
struct Override { appkit.Layer }
fn (o Override) read() int { return 77 }
struct Shadow { appkit.Layer
	value int = 91
}
fn invoke(callback fn ()) { callback() }
fn call(callback fn () int) int { return callback() }
fn main() {
	app := &App{Layer: appkit.Layer{App: appkit.App{value: 42}}}
	invoke(fn [app] () {
		value := app.value
		assert value == 42
	})
	read := app.read
	assert read() == 42
	assert call(app.read) == 42
	assert app.local_only() == 999
	override := Override{Layer: app.Layer}
	assert call(override.read) == 77
	shadow := Shadow{Layer: app.Layer}
	assert shadow.value == 91
	assert shadow.Layer.value == 42
	imported := appkit.Imported{Base: model.Base{value: 53}}
	assert call(imported.read) == 53
	selected := appkit.Selection{Selected: model.Selected{value: 59}}
	assert call(selected.read) == 59
	generic := appkit.Generic[int]{Holder: appkit.Holder[int]{value: 71}}
	assert generic.read() == 71
}
')!
	return root
}

fn test_embedded_receiver_lookup_uses_the_declaring_file() {
	root := embedded_receiver_scope_fixture('lookup')!
	defer { os.rmdir_all(root) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_files([os.join_path(root, 'model', 'model.v'),
		os.join_path(root, 'appkit', 'appkit.v'), os.join_path(root, 'appkit', 'selected.v'),
		os.join_path(root, 'main.v')])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.collect_types()
	t.rebuild_embedded_fields_index()
	t.cur_module = 'main'
	t.cur_file = os.join_path(root, 'main.v')
	for name in ['App.local_only', 'appkit.App.read', 'model.Base.read', 'appkit.Callback.read'] {
		t.fn_ret_types.set_id(name, flat.source_type_int)
	}
	assert t.resolve_embedded_receiver_method(flat.source_type_id('App'), 'read') or { '' } == 'appkit.App.read'
	assert t.resolve_embedded_receiver_method(flat.source_type_id('appkit.Imported'), 'read') or { '' } == 'model.Base.read'
	assert t.resolve_embedded_receiver_method(flat.source_type_id('appkit.CallbackLayer'), 'read') or { '' } == 'appkit.Callback.read'
	assert t.resolve_embedded_receiver_method(flat.source_type_id('App'), 'local_only') == none
	assert t.resolve_embedded_receiver_method(flat.source_type_id('App'), 'missing') == none
	path := t.embedded_receiver_path(flat.source_type_id('App'), flat.source_type_id('appkit.App')) or { panic('missing imported receiver path') }
	assert path.map(it.name) == ['appkit.Layer', 'App']
	assert t.embedded_receiver_path(flat.source_type_id('App'), flat.source_type_id('App')) == none
	selected_info := t.structs[flat.source_type_id('appkit.Selection')]
	selected := selected_info.fields[0]
	assert flat.source_type_name(t.embedded_receiver_field_type(flat.source_type_id('appkit.Selection'), selected)) == 'model.Selected'
	generic := t.lookup_struct_info(flat.source_type_id('appkit.Generic[int]')) or { panic('missing generic owner') }
	assert flat.source_type_name(t.embedded_receiver_field_type(flat.source_type_id('appkit.Generic[int]'), generic.fields[0])) == 'appkit.Holder[int]'
}

fn test_embedded_receiver_lookup_terminates_on_cyclic_metadata() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	for owner, embedded in {
		'First':  'Second'
		'Second': 'First'
	} {
		field := FieldInfo{
			name:        embedded
			typ:         flat.source_type_id(embedded)
			raw_typ:     flat.source_type_id(embedded)
			is_embedded: true
		}
		t.structs[flat.source_type_id(owner)] = StructInfo{
			name:   flat.source_type_id(owner)
			module: 'main'
			fields: [field]
		}
		t.embedded_fields[flat.source_type_id(owner)] = [field]
	}
	assert t.resolve_embedded_receiver_method(flat.source_type_id('First'), 'missing') == none
	assert t.embedded_receiver_path(flat.source_type_id('First'), flat.source_type_id('Absent')) == none
}

fn test_embedded_receiver_closures_keep_imported_storage_and_method_dispatch() {
	root := embedded_receiver_scope_fixture('native')!
	defer { os.rmdir_all(root) or {} }
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', @CCOMPILER, 'run',
		root])
	assert result.exit_code == 0, result.output
}
