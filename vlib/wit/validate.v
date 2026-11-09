module wit

pub struct WitError {
pub:
	code    string
	message string
	line    int
}

pub fn validate(doc WitDoc) ! {
	// Collect defined type names
	mut types := map[string]bool{}
	for iface in doc.interfaces {
		for it in iface.items {
			match it.kind {
				.record { if r := it.record { types[r.name] = true } }
				.enum_ { if e := it.enum_ { types[e.name] = true } }
				.variant { if v := it.variant { types[v.name] = true } }
				.flags { if f := it.flags { types[f.name] = true } }
				.resource { if r := it.resource { types[r.name] = true } }
				.type_alias { if t := it.type_def { types[t.name] = true } }
				else {}
			}
		}
	}
	// primitive types
	for p in ['bool', 'u8', 'u16', 'u32', 'u64', 's8', 's16', 's32', 's64',
		'f32', 'f64', 'char', 'string'] {
		types[p] = true
	}
	// check duplicate interface names
	mut iface_names := map[string]bool{}
	for iface in doc.interfaces {
		if iface.name in iface_names {
			return error('error_duplicate_name: duplicate interface "${iface.name}"')
		}
		iface_names[iface.name] = true
	}
	// check duplicate world names
	mut world_names := map[string]bool{}
	for w in doc.worlds {
		if w.name in world_names {
			return error('error_duplicate_name: duplicate world "${w.name}"')
		}
		world_names[w.name] = true
	}
	// check each interface
	for iface in doc.interfaces {
		// duplicate item names
		mut item_names := map[string]bool{}
		for it in iface.items {
			mut name := ''
			match it.kind {
				.func { if f := it.func { name = f.name } }
				.record { if r := it.record { name = r.name } }
				.variant { if v := it.variant { name = v.name } }
				.enum_ { if e := it.enum_ { name = e.name } }
				.flags { if f := it.flags { name = f.name } }
				.resource { if r := it.resource { name = r.name } }
				.type_alias { if t := it.type_def { name = t.name } }
				else {}
			}
			if name != '' {
				if name in item_names {
					return error('error_duplicate_name: duplicate name "${name}" in interface "${iface.name}"')
				}
				item_names[name] = true
			}
			// check types in records
			if it.kind == .record {
				if r := it.record {
					for f in r.fields {
						check_type(f.typ, types)!
					}
				}
			}
			// check types in funcs
			if it.kind == .func {
				if f := it.func {
					for p in f.params {
						check_type(p.typ, types)!
					}
					for r in f.results {
						check_type(r, types)!
					}
				}
			}
			// check type alias
			if it.kind == .type_alias {
				if t := it.type_def {
					check_type(t.typ, types)!
				}
			}
			// check resource methods
			if it.kind == .resource {
				if r := it.resource {
					mut method_names := map[string]bool{}
					for m in r.methods {
						if m.name in method_names {
							return error('error_duplicate_name: duplicate method "${m.name}" in resource "${r.name}"')
						}
						method_names[m.name] = true
						// constructor must not take the resource; static must not either
						// first param of non-static method should be borrow of resource (optional check)
						for p in m.params { check_type(p.typ, types)! }
						for res in m.results { check_type(res, types)! }
					}
				}
			}
		}
	}
}

fn check_type(t WitType, types map[string]bool) ! {
	match t.kind {
		.prim {}
		.named {
			n := t.name
			// strip generic args for checking base name
			base := if n.contains('<') { n.all_before('<') } else { n }
			if base !in types {
				return error('error_unknown_type: unknown type "${base}"')
			}
		}
		.list {
			if t.items.len > 0 { check_type(t.items[0], types)! }
		}
		.option {
			if t.items.len > 0 { check_type(t.items[0], types)! }
		}
		.result {
			for it in t.items { check_type(it, types)! }
		}
		.tuple {
			for it in t.items { check_type(it, types)! }
		}
	}
}