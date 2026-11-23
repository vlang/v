module wit

import wasm

// wit_cabi_size_align computes the canonical-ABI (size, align) of a WIT
// type, resolving named types (records, variants, enums, flags, aliases)
// through doc. Returns (-1, -1) when the type is unresolvable or involves a
// resource/handle, which has no fixed guest layout. Recursion deeper than 8
// levels is refused the same way. Primitives follow the Canonical ABI
// element-size table; strings and lists are (ptr, len) pairs; options,
// results, variants, records, and tuples follow the union/struct rules in
// wasm.cabi_*_layout.
pub fn wit_cabi_size_align(t WitType, doc WitDoc) (int, int) {
	return wit_cabi_size_align_inner(t, doc, 0)
}

fn wit_cabi_size_align_inner(t WitType, doc WitDoc, depth int) (int, int) {
	if depth > 8 {
		return -1, -1
	}
	match t.kind {
		.prim {
			if t.prim == 'string' {
				return wasm.cabi_string_layout()
			}
			return wasm.cabi_primitive_size_align(t.prim)
		}
		.list {
			return wasm.cabi_list_layout()
		}
		.option {
			if t.items.len == 0 {
				return -1, -1
			}
			ps, pa := wit_cabi_size_align_inner(t.items[0], doc, depth + 1)
			if ps < 0 {
				return -1, -1
			}
			return wasm.cabi_option_layout(ps, pa)
		}
		.result {
			mut oks := 0
			mut oka := 1
			mut ers := 0
			mut era := 1
			if t.items.len > 0 {
				oks, oka = wit_cabi_size_align_inner(t.items[0], doc, depth + 1)
				if oks < 0 {
					return -1, -1
				}
			}
			if t.items.len > 1 {
				ers, era = wit_cabi_size_align_inner(t.items[1], doc, depth + 1)
				if ers < 0 {
					return -1, -1
				}
			}
			return wasm.cabi_result_layout(oks, oka, ers, era)
		}
		.tuple {
			mut fields := []wasm.CabiField{cap: t.items.len}
			for it in t.items {
				s, a := wit_cabi_size_align_inner(it, doc, depth + 1)
				if s < 0 {
					return -1, -1
				}
				fields << wasm.CabiField{ name: '', size: s, align: a }
			}
			_, size, align := wasm.cabi_tuple_layout(fields)
			return size, align
		}
		.named {
			return wit_named_cabi_size_align(t.name, doc, depth)
		}
	}
}

fn wit_named_cabi_size_align(name string, doc WitDoc, depth int) (int, int) {
	if name == '_' || name == '' {
		return 0, 1
	}
	if name.starts_with('borrow<') || name.starts_with('own<') {
		return -1, -1
	}
	if name == 'pollable' {
		return 4, 4
	}
	// Search all interfaces for a matching definition.
	for iface in doc.interfaces {
		for it in iface.items {
			match it.kind {
				.record {
					if r := it.record {
						if r.name == name {
							mut fields := []wasm.CabiField{cap: r.fields.len}
							for f in r.fields {
								s, a := wit_cabi_size_align_inner(f.typ, doc, depth + 1)
								if s < 0 {
									return -1, -1
								}
								fields << wasm.CabiField{ name: f.name, size: s, align: a }
							}
							_, size, align := wasm.cabi_record_layout(fields)
							return size, align
						}
					}
				}
				.enum_ {
					if e := it.enum_ {
						if e.name == name {
							return wasm.cabi_enum_layout(e.cases.len)
						}
					}
				}
				.flags {
					if f := it.flags {
						if f.name == name {
							return wasm.cabi_flags_layout(f.flags.len)
						}
					}
				}
				.variant {
					if v := it.variant {
						if v.name == name {
							mut cases := []wasm.CabiField{cap: v.cases.len}
							for c in v.cases {
								if pt := c.typ {
									s, a := wit_cabi_size_align_inner(pt, doc, depth + 1)
									if s < 0 {
										return -1, -1
									}
									cases << wasm.CabiField{ name: c.name, size: s, align: a }
								} else {
									cases << wasm.CabiField{ name: c.name, size: 0, align: 1 }
								}
							}
							_, size, align := wasm.cabi_variant_layout(v.cases.len, cases)
							return size, align
						}
					}
				}
				.type_alias {
					if td := it.type_def {
						if td.name == name {
							return wit_cabi_size_align_inner(td.typ, doc, depth + 1)
						}
					}
				}
				else {}
			}
		}
	}
	// Unknown names (futures/streams mapped to voidptr in codegen, externals)
	// have no canonical guest layout.
	return -1, -1
}
