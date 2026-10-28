module driver

import v.flat

fn test_implicit_type_index_replays_numeric_keys_and_clones_bindings() {
	owner := flat.source_type_id('driver_numeric_index.Owner')
	alias := flat.source_type_id('driver_numeric_index.Alias')
	mut fields := flat.TypeTextBindings{}
	fields.set_id('value', flat.source_type_int)
	mut index := ImplicitFieldScanIndex{}
	index.apply(ImplicitIndexOp{
		kind:    .fields
		type_id: owner
		fields:  fields
	})
	index.apply(ImplicitIndexOp{
		kind:        .enum_fields
		type_id:     owner
		enum_fields: {
			'member': true
		}
	})
	index.apply(ImplicitIndexOp{
		kind:    .alias
		type_id: alias
		value:   owner
	})
	assert implicit_field_type(alias, 'value', index) == flat.source_type_int
	assert index.enum_fields[owner]['member']
	fields.set_id('value', flat.source_type_string)
	assert implicit_field_type(owner, 'value', index) == flat.source_type_int
	mut other := index.clone()
	other.aliases.set_id(alias, flat.source_type_string)
	assert implicit_normalize_type(alias, index) == owner
	assert implicit_normalize_type(alias, other) == flat.source_type_string
	index.apply(ImplicitIndexOp{
		kind:    .alias
		type_id: alias
		value:   0
	})
	assert index.aliases.type_id(alias) or { u32(1) } == 0
}

fn test_implicit_type_index_dependencies_keep_kind_identity_and_write_order() {
	owner := flat.source_type_id('driver_numeric_dependencies.Owner')
	record := &ImplicitIndexRecord{}
	index := ImplicitFieldScanIndex{ record: record }
	for op in [
		ImplicitIndexOp{ kind: .global, key: 'first' },
		ImplicitIndexOp{ kind: .fields, type_id: owner },
		ImplicitIndexOp{ kind: .enum_fields, type_id: owner },
		ImplicitIndexOp{ kind: .fields, type_id: owner },
	] {
		index.write(op)
	}
	assert record.writes.len == 3
	mut prepared := PreparedScan{
		ready:      true
		type_reads: {
			implicit_type_index_key(.enum_fields, owner): true
		}
	}
	scan := ImplicitImportScan{ field_index: index }
	assert prepared.replay_conflict(scan, 0) == 'the program writes `e:driver_numeric_dependencies.Owner`'
	prepared.type_reads[implicit_type_index_key(.fields, owner)] = true
	assert prepared.replay_conflict(scan, 0) == 'the program writes `f:driver_numeric_dependencies.Owner`'
	prepared.reads['g:first'] = true
	assert prepared.replay_conflict(scan, 0) == 'the program writes `g:first`'
	assert implicit_type_index_key(.fields, 0) != implicit_type_index_key(.enum_fields, 0)
}

fn test_implicit_type_probe_misses_only_persist_when_recording_dependencies() {
	missing := 'driver_numeric_probe_missing_7b8c'
	assert flat.source_type_existing_id(missing) == none
	index := ImplicitFieldScanIndex{}
	assert index.type_name_id(missing) == none
	assert flat.source_type_existing_id(missing) == none
	record := &ImplicitIndexRecord{ with_reads: true }
	recorded := ImplicitFieldScanIndex{ record: record }
	missing_id := recorded.type_name_id(missing) or { panic('missing recorded dependency') }
	recorded.read_type(.fields, missing_id)
	assert implicit_type_index_key(.fields, missing_id) in record.type_reads
	assert flat.source_type_existing_id(missing) or { u32(0) } == missing_id
}
