module main

// generated_idents lists every name the emitter introduces into the output that
// is not derived from the schema: local variables, function parameters, and the
// alias the runtime is imported under.
//
// A generated local may not share the module's own name. V rejects the
// declaration outright, with a diagnostic that points at the local rather than
// at the module, so the error names the generator's identifier and gives no hint
// that the module name is the real cause. Checking it here turns that into a
// message about `-m`.
//
// Keeping this list by hand means a new local has to be added, which is the
// point: the alternative is a module name that silently breaks output.
pub const generated_idents = ['packer', 'unpacker', 'out', 'msg', 'opts', 'sub', 'nested', 'entry',
	'entry_key', 'entry_value', 'keys', 'number', 'wire_type', 'part', 'part_wire', 'payload',
	'data', 'field_number', 'map_data', 'inner', 'item']

// runtime_alias is the name the generated file imports the runtime under.
pub const runtime_alias = 'protobuf'

// module_name_conflict returns a description of what `module` collides with, or
// an empty string when it is usable.
//
// Two kinds of collision are checked. A local that shadows the module name makes
// the output fail to compile. A module named `protobuf` cannot import the runtime
// at all, since the import would land in a module of the same name.
pub fn module_name_conflict(module string, res &ResolvedFile) string {
	if module == '' {
		return ''
	}
	if module == runtime_alias {
		return 'it would have to import `encoding.protobuf` into a module of the same name'
	}
	for ident in generated_idents {
		if ident == module {
			return 'the generated code declares a local called `${ident}`, and V rejects a local whose name is the module name'
		}
	}
	// A type of the same name is the same collision: the declaration would be a
	// second definition of the module.
	for m in res.messages {
		if m.v_name == module {
			return 'the schema declares a message of the same name'
		}
	}
	for en in res.enums {
		if en.v_name == module {
			return 'the schema declares an enum of the same name'
		}
	}
	return ''
}

// module_name_error returns the full message explaining why `module` cannot be
// used, or an empty string when it can.
pub fn module_name_error(module string, res &ResolvedFile) string {
	conflict := module_name_conflict(module, res)
	if conflict == '' {
		return ''
	}
	return 'pbgen: module name `${module}` cannot be used, because ${conflict}. Pass a different name with `-m`.'
}

// check_module_name returns an error naming `-m` when `module` cannot be used.
pub fn check_module_name(module string, res &ResolvedFile) ! {
	msg := module_name_error(module, res)
	if msg == '' {
		return
	}
	return error(msg)
}
