module fastc

import strings
import v.flat
import v.pref

fn test_fastc_comptime_signatures_preserve_positions_with_recorded_body_spans() {
	source := r'module main
struct State { value int }
fn before(value int) int {
	$if true { return value }
	return 0
}
$if picked ? {
	fn selected(value int) int { return value }
	$if false {
		fn inactive_nested(value Missing) Missing { return value }
	} $else {
		pub fn (state State) active_method(value string) !int { return state.value }
		fn nested_free(mut values []int) int { return values.len }
	}
} $else {
	fn fallback() bool { return true }
}
fn (state State) after(value int) int {
	if value > 0 { return state.value }
	return value
}
$if false {
	fn inactive_outer(value Missing) Missing { return value }
} $else $if true {
	fn final_selected() string { return "selected" }
}
fn tail() {}
'
	path := 'comptime_body_spans.v'
	for picked in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.building_v = true
		if picked { prefs.user_defines << 'picked' }
		mut declared := map[u32]bool{}
		mut kinds := map[u32]FastcDeclaredTypeKind{}
		mut enum_flags := map[u32]bool{}
		mut params_structs := map[string]bool{}
		mut declaration_paths := map[u32]string{}
		mut declaration_modules := map[u32]string{}
		mut type_source := strings.new_builder(128)
		mut source_origins := []FastcSourceOrigin{}
		mut body_spans := []int{}
		collect_declared_types(source, path, 'main', prefs, mut declared, mut kinds,
			mut enum_flags, mut params_structs, mut declaration_paths, mut declaration_modules,
			mut type_source, mut source_origins, mut body_spans) or { panic(err) }
		// The test keeps the recorded spans, not the generated declaration text.
		unsafe { type_source.free() }
		assert body_spans.len == 6
		header := FastcSourceHeader{
			module_name:     'main'
			has_comptime_if: true
			body_spans:      body_spans
		}
		mut full := map[string]FastcFunctionSignature{}
		mut skipped := map[string]FastcFunctionSignature{}
		collect_function_signatures(source, path, header, prefs, []int{}, declared,
			flat.TypeTextBindings{}, params_structs, mut full) or { panic(err) }
		collect_function_signatures(source, path, header, prefs, body_spans, declared,
			flat.TypeTextBindings{}, params_structs, mut skipped) or { panic(err) }
		assert skipped.len == full.len
		for key, signature in full {
			actual := skipped[key] or { panic('missing signature ${key}') }
			assert actual == signature, key
		}
		mut expected := {
			'before':         'fn before('
			'State.after':    'fn (state State) after('
			'final_selected': 'fn final_selected('
			'tail':           'fn tail('
		}
		if picked {
			expected['selected'] = 'fn selected('
			expected['State.active_method'] = 'fn (state State) active_method('
			expected['nested_free'] = 'fn nested_free('
		} else {
			expected['fallback'] = 'fn fallback('
		}
		assert skipped.len == expected.len
		for key, declaration in expected {
			signature := skipped[key] or { panic('missing active signature ${key}') }
			assert signature.position == source.index(declaration) or { panic(declaration) }, key
			assert signature.path == path
		}
		assert 'inactive_nested' !in skipped
		assert 'inactive_outer' !in skipped
		if picked {
			method := skipped['State.active_method'] or { panic('missing active method') }
			assert method.is_public
			assert method.parameter_types.names() == ['State', 'string']
			assert method.option_type_text() == 'int'
		}
	}
}

fn test_fastc_selected_signature_slices_ignore_original_header_body_spans() {
	source := r'fn first() {}
$if true {
	fn nested() int { return 42 }
}
fn last() {}
'
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	// Original-file spans must not become slice-relative skip coordinates. This
	// stale span deliberately overlaps the nested branch if reused by the helper.
	header := FastcSourceHeader{
		module_name:     'main'
		has_comptime_if: true
		body_spans:      [source.index('{') or { panic('missing first body') }, source.len]
	}
	mut signatures := map[string]FastcFunctionSignature{}
	collect_function_signatures_at(source, 137, 'selected_slice.v', header, prefs, []int{},
		map[u32]bool{}, flat.TypeTextBindings{}, map[string]bool{}, mut signatures) or {
		panic(err)
	}
	assert signatures.len == 3
	for name in ['first', 'nested', 'last'] {
		signature := signatures[name] or { panic('missing slice signature ${name}') }
		assert signature.position == 137 + source.index('fn ${name}(') or { panic(name) }
	}
}
