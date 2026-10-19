module fastc

import v.flat
import v.pref
import v.scanner

fn fastc_metadata_test_parser(fields map[string][]FastcStructField, lookup map[string]map[string]&FastcStructField) Parser {
	prefs := pref.new_preferences()
	return Parser{
		prefs:               prefs
		s:                   scanner.new_scanner(prefs, .normal)
		struct_field_info:   fields
		struct_field_lookup: lookup
	}
}

fn fastc_metadata_test_fields() map[string][]FastcStructField {
	mut leaf_fields := [FastcStructField{
		name:          'value'
		typ:           flat.source_type_int
		is_public:     true
		module_name:   'metadata'
		default_value: '41'
	}]
	for i in 0 .. 192 {
		leaf_fields << FastcStructField{
			name: 'item_${i}'
			typ:  flat.source_type_int
		}
	}
	return {
		'MetadataLeaf':  leaf_fields
		'MetadataMid':   [FastcStructField{
			name: '__embedded_0'
			typ:  flat.source_type_id('MetadataLeaf')
		}]
		'MetadataRoot':  [FastcStructField{
			name: '__embedded_1'
			typ:  flat.source_type_id('MetadataMid')
		}]
		'MetadataOther': [FastcStructField{
			name: '__embedded_2'
			typ:  flat.source_type_id('MetadataLeaf')
		}]
	}
}

fn test_fastc_direct_field_metadata_owns_stable_shared_records() {
	fields := fastc_metadata_test_fields()
	lookup := fastc_build_struct_field_lookup(fields)
	mut g := fastc_metadata_test_parser(fields, lookup)
	first := g.struct_field_metadata(flat.source_type_id('MetadataLeaf'), 'value') or { panic('missing direct field') }
	assert first.typ == flat.source_type_int
	assert first.default_value == '41'
	assert first.module_name == 'metadata'
	assert first.storage_path.len == 0
	// The index owns its record rather than pointing into the source array or
	// the loop variable used to construct it.
	mut source_fields := fields['MetadataLeaf'] or { panic('missing source fields') }
	source_fields[0].default_value = '99'
	assert first.default_value == '41'
	for i in 0 .. 192 {
		field := g.struct_field_metadata(flat.source_type_id('MetadataLeaf*'), 'item_${i}') or {
			panic('missing direct field ${i}')
		}
		assert field.name == 'item_${i}'
	}
	again := g.struct_field_metadata(flat.source_type_id('MetadataLeaf'), 'value') or { panic('missing direct field') }
	assert voidptr(first) == voidptr(again)
	g.reset_lookup_memos()
	after_reset := g.struct_field_metadata(flat.source_type_id('MetadataLeaf'), 'value') or {
		panic('missing direct field after reset')
	}
	assert voidptr(first) == voidptr(after_reset)
	other := fastc_metadata_test_parser(fields, lookup)
	shared := other.struct_field_metadata(flat.source_type_id('MetadataLeaf'), 'value') or {
		panic('missing shared field')
	}
	assert voidptr(shared) == voidptr(first)
}

fn test_fastc_promoted_field_metadata_preserves_paths_and_memo_pointers() {
	fields := fastc_metadata_test_fields()
	mut g := fastc_metadata_test_parser(fields, fastc_build_struct_field_lookup(fields))
	leaf := g.struct_field_metadata(flat.source_type_id('MetadataLeaf'), 'value') or { panic('missing leaf field') }
	middle := g.struct_field_metadata(flat.source_type_id('MetadataMid'), 'value') or { panic('missing middle field') }
	root := g.struct_field_metadata(flat.source_type_id('MetadataRoot'), 'value') or { panic('missing root field') }
	other := g.struct_field_metadata(flat.source_type_id('MetadataOther'), 'value') or { panic('missing other field') }
	assert voidptr(middle) != voidptr(leaf)
	assert voidptr(root) != voidptr(middle)
	assert voidptr(other) != voidptr(middle)
	assert leaf.storage_path.len == 0
	assert middle.storage_path == ['__embedded_0']
	assert root.storage_path == ['__embedded_1', '__embedded_0']
	assert other.storage_path == ['__embedded_2']
	assert root.default_value == '41'
	// Growing both nested memo maps must not move previously returned records.
	for i in 0 .. 192 {
		field := g.struct_field_metadata(flat.source_type_id('MetadataRoot'), 'item_${i}') or {
			panic('missing promoted field ${i}')
		}
		assert field.storage_path == root.storage_path
	}
	again := g.struct_field_metadata(flat.source_type_id('MetadataRoot'), 'value') or { panic('missing memoized field') }
	assert voidptr(again) == voidptr(root)
	last := g.struct_field_metadata(flat.source_type_id('MetadataRoot'), 'value') or { panic('missing last field') }
	assert voidptr(last) == voidptr(root)
	assert g.struct_field_metadata(flat.source_type_id('MetadataRoot'), 'absent') == none
	assert g.last_field_known
	assert g.last_field == unsafe { nil }
	assert g.struct_field_metadata(flat.source_type_id('MetadataRoot'), 'absent') == none
	_ = g.struct_field_metadata(flat.source_type_id('MetadataLeaf'), 'value') or { panic('missing leaf field') }
	assert g.struct_field_metadata(flat.source_type_id('MetadataRoot'), 'absent') == none
	if misses := g.field_memo[flat.source_type_id('MetadataRoot')] {
		missing := misses['absent'] or { panic('missing negative memo') }
		assert missing == unsafe { nil }
	} else {
		panic('missing receiver memo')
	}
	g.reset_lookup_memos()
	assert !g.last_field_known
	assert g.field_memo.len == 0
	rebuilt := g.struct_field_metadata(flat.source_type_id('MetadataRoot'), 'value') or { panic('missing rebuilt field') }
	assert voidptr(rebuilt) != voidptr(root)
	assert rebuilt.storage_path == root.storage_path
	assert leaf.storage_path.len == 0
	assert middle.storage_path == ['__embedded_0']
}

fn test_fastc_shared_field_metadata_survives_parser_scope_release() {
	$if prealloc {
		fields := fastc_metadata_test_fields()
		lookup := fastc_build_struct_field_lookup(fields)
		mut shared := unsafe { &FastcStructField(nil) }
		for _ in 0 .. 2 {
			scope := unsafe { prealloc_scope_begin() }
			g := fastc_metadata_test_parser(fields, lookup)
			direct := g.struct_field_metadata(flat.source_type_id('MetadataLeaf'), 'value') or {
				panic('missing shared field')
			}
			promoted := g.struct_field_metadata(flat.source_type_id('MetadataRoot'), 'value') or {
				panic('missing scoped field')
			}
			assert !unsafe { prealloc_scope_owns(scope, direct) }
			assert unsafe { prealloc_scope_owns(scope, promoted) }
			assert unsafe { prealloc_scope_owns(scope, promoted.storage_path.data) }
			assert direct.storage_path.len == 0
			assert promoted.storage_path == ['__embedded_1', '__embedded_0']
			if shared == unsafe { nil } {
				shared = direct
			} else {
				assert voidptr(direct) == voidptr(shared)
			}
			unsafe { prealloc_scope_leave(scope) }
			unsafe { prealloc_scope_free_after(scope) }
			assert shared.default_value == '41'
			assert shared.storage_path.len == 0
		}
	}
}
