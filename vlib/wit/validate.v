module wit

pub struct WitError {
pub:
	code    string
	message string
	line    int
}

fn wit_since_of(attrs []string) (int, int, int) {
	for a in attrs {
		prefix := '@since(version = '
		if a.starts_with(prefix) && a.ends_with(')') {
			inner := a[prefix.len..a.len - 1]
			parts := inner.split('.')
			if parts.len == 3 {
				return parts[0].int(), parts[1].int(), parts[2].int()
			}
		}
	}
	return 0, 0, 0
}

fn wit_version_newer(a0 int, a1 int, a2 int, b0 int, b1 int, b2 int) bool {
	if a0 != b0 {
		return a0 > b0
	}
	if a1 != b1 {
		return a1 > b1
	}
	return a2 > b2
}

fn wit_find_interface(doc WitDoc, path string) int {
	mut base := path
	if base.contains('@') {
		base = base.all_before('@')
	}
	for i, iface in doc.interfaces {
		if iface.name == path || iface.name == base || iface.name == base.all_after_last('/') {
			return i
		}
	}
	return -1
}

fn wit_use_member(entry string) string {
	if entry.contains(' as ') {
		return entry.all_before(' as ').trim_space()
	}
	return entry.trim_space()
}

fn wit_item_version(items []WitItem, name string, d0 int, d1 int, d2 int) []int {
	for it in items {
		mut iname := ''
		mut iattrs := []string{}
		match it.kind {
			.func {
				if f := it.func {
					iname = f.name
					iattrs = f.attrs
				}
			}
			.record {
				if r := it.record {
					iname = r.name
				}
			}
			.variant {
				if v := it.variant {
					iname = v.name
				}
			}
			.enum_ {
				if e := it.enum_ {
					iname = e.name
				}
			}
			.flags {
				if f := it.flags {
					iname = f.name
				}
			}
			.resource {
				if r := it.resource {
					iname = r.name
				}
			}
			.type_alias {
				if t := it.type_def {
					iname = t.name
				}
			}
			else {}
		}
		if iname == name {
			v0, v1, v2 := wit_since_of(it.attrs)
			if v0 != 0 || v1 != 0 || v2 != 0 {
				return [v0, v1, v2]
			}
			a, b, c := wit_since_of(iattrs)
			if a != 0 || b != 0 || c != 0 {
				return [a, b, c]
			}
			return [d0, d1, d2]
		}
	}
	return []
}

pub fn validate(doc WitDoc) ! {
	// Collect defined type names
	mut types := map[string]bool{}
	for iface in doc.interfaces {
		for it in iface.items {
			match it.kind {
				.record {
					if r := it.record {
						types[r.name] = true
					}
				}
				.enum_ {
					if e := it.enum_ {
						types[e.name] = true
					}
				}
				.variant {
					if v := it.variant {
						types[v.name] = true
					}
				}
				.flags {
					if f := it.flags {
						types[f.name] = true
					}
				}
				.resource {
					if r := it.resource {
						types[r.name] = true
					}
				}
				.type_alias {
					if t := it.type_def {
						types[t.name] = true
					}
				}
				else {}
			}
		}
	}
	mut resources := map[string]bool{}
	for iface in doc.interfaces {
		for it in iface.items {
			if it.kind == .resource {
				if r := it.resource {
					resources[r.name] = true
				}
			}
		}
	}
	// primitive types
	for p in ['bool', 'u8', 'u16', 'u32', 'u64', 's8', 's16', 's32', 's64', 'f32', 'f64', 'char',
		'string'] {
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
				.func {
					if f := it.func {
						name = f.name
					}
				}
				.record {
					if r := it.record {
						name = r.name
					}
				}
				.variant {
					if v := it.variant {
						name = v.name
					}
				}
				.enum_ {
					if e := it.enum_ {
						name = e.name
					}
				}
				.flags {
					if f := it.flags {
						name = f.name
					}
				}
				.resource {
					if r := it.resource {
						name = r.name
					}
				}
				.type_alias {
					if t := it.type_def {
						name = t.name
					}
				}
				else {}
			}
			if name != '' {
				if name in item_names {
					return error('error_duplicate_name: duplicate name "${name}" in interface "${iface.name}"')
				}
				item_names[name] = true
			}
			// check use items: target must exist, members must resolve, versions must hold
			if it.kind == .use {
				if u := it.use_ {
					target := wit_find_interface(doc, u.path)
					if target < 0 {
						return error('error_unknown_interface: use of unknown interface "${u.path}"')
					}
					iv0, iv1, iv2 := wit_since_of(iface.attrs)
					tv0, tv1, tv2 := wit_since_of(doc.interfaces[target].attrs)
					for entry in u.items {
						mname := wit_use_member(entry)
						ver := wit_item_version(doc.interfaces[target].items, mname, tv0, tv1, tv2)
						if ver.len == 0 {
							return error('error_unresolved_use: "${mname}" not found in interface "${u.path}"')
						}
						v0, v1, v2 := ver[0], ver[1], ver[2]
						if wit_version_newer(v0, v1, v2, iv0, iv1, iv2) {
							return error('error_version_incompatible: use of "${mname}" at ${v0}.${v1}.${v2} exceeds importer "${iface.name}" at ${iv0}.${iv1}.${iv2}')
						}
					}
				}
			}
			// check types in records
			if it.kind == .record {
				if r := it.record {
					for f in r.fields {
						check_type(f.typ, types, resources)!
					}
				}
			}
			// check types in funcs
			if it.kind == .func {
				if f := it.func {
					for p in f.params {
						check_type(p.typ, types, resources)!
					}
					for r in f.results {
						check_type(r, types, resources)!
					}
				}
			}
			// check type alias
			if it.kind == .type_alias {
				if t := it.type_def {
					check_type(t.typ, types, resources)!
				}
			}
			// check resource methods
			if it.kind == .resource {
				if r := it.resource {
					mut method_names := map[string]bool{}
					mut ctors := 0
					for m in r.methods {
						if m.name == 'constructor' {
							ctors++
							if ctors > 1 {
								return error('error_duplicate_constructor: duplicate constructor in resource "${r.name}"')
							}
						}
						if m.name in method_names {
							return error('error_duplicate_name: duplicate method "${m.name}" in resource "${r.name}"')
						}
						method_names[m.name] = true
						if m.is_static {
							for p in m.params {
								if is_bare_resource_ref(p.typ, r.name) {
									return error('error_static_method_takes_resource: static method "${m.name}" must not take resource "${r.name}"')
								}
							}
						}
						for p in m.params {
							check_type(p.typ, types, resources)!
						}
						for res in m.results {
							check_type(res, types, resources)!
						}
					}
				}
			}
		}
	}
}

fn is_bare_resource_ref(t WitType, rname string) bool {
	if t.kind != .named {
		return false
	}
	return t.name == rname
}

fn check_type(t WitType, types map[string]bool, resources map[string]bool) ! {
	match t.kind {
		.prim {}
		.named {
			n := t.name
			// strip generic args for checking base name
			base := if n.contains('<') { n.all_before('<') } else { n }
			if base == 'own' || base == 'borrow' {
				inner := n.all_after('<').all_before('>')
				if inner !in resources {
					return error('error_not_a_resource: ${base} of non-resource "${inner}"')
				}
				return
			}
			if base !in types {
				return error('error_unknown_type: unknown type "${base}"')
			}
		}
		.list {
			if t.items.len > 0 {
				check_type(t.items[0], types, resources)!
			}
		}
		.option {
			if t.items.len > 0 {
				check_type(t.items[0], types, resources)!
			}
		}
		.result {
			for it in t.items {
				check_type(it, types, resources)!
			}
		}
		.tuple {
			for it in t.items {
				check_type(it, types, resources)!
			}
		}
	}
}
