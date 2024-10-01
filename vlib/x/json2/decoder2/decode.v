module decoder2

import time

// Node represents a node in a JSON decoder tree.
struct Node {
	key_pos  int     // The position of the key in the JSON string.
	key_len  int     // The length of the key in the JSON string.
	children ?[]Node // The children nodes of the current node.
}

// Decoder represents a JSON decoder.
struct Decoder {
	json string // json is the JSON data to be decoded.
mut:
	idx int // idx is the current index of the decoder.
}

pub enum ValueKind {
	unknown
	array
	object
	string_
	number
	boolean
}

// check_json checks if the JSON string is valid.
fn check_json(val string) ! {
	if val == '' {
		return error('empty string')
	}
}

// decode decodes a JSON string into a specified type.
pub fn decode[T](val string) !T {
	check_json(val)!

	mut nodes := []Node{}
	mut decoder := Decoder{
		json: val
	}

	// TODO: needs performance improvements
	decoder.fulfill_nodes(mut nodes)

	mut result := T{}
	decoder.decode_value(nodes, &result)
	return result
}

// decode_value decodes a value from the JSON nodes.
fn (mut decoder Decoder) decode_value[T](nodes []Node, val &T) {
	$if val is $option {
	} $else $if T is string {
	} $else $if T is $sumtype {
		$for v in val.variants {
			if val is v {
				decoder.decode_value(nodes, val)
			}
		}
	} $else $if T is $alias {
	} $else $if T is time.Time {
	} $else $if T is $map {
	} $else $if T is $array {
	} $else $if T is $struct {
		decoder.decode_struct(nodes, val)
	} $else $if T is $enum {
	} $else $if T is $int {
	} $else $if T is $float {
	} $else $if T is bool {
	} $else {
		return error('cannot encode value with ${typeof(val).name} type')
	}
}

// get_value_kind returns the kind of a JSON value.
fn get_value_kind(value rune) ValueKind {
	return match value {
		`"` { .string_ }
		`t`, `f` { .boolean }
		`{` { .object }
		`[` { .array }
		`0`...`9` { .number }
		else { .unknown }
	}
}

// decode_optional_value_in_actual_node decodes an optional value in a node.
fn (mut decoder Decoder) decode_optional_value_in_actual_node[T](node Node, val ?T) T {
	start := (node.key_pos + node.key_len) + 3
	mut end := start
	for decoder.json[end] != `,` && decoder.json[end] != `}` {
		end++
	}
	mut value_kind := get_value_kind(decoder.json[start])

	$if T is string {
		if value_kind == .string_ {
			return decoder.json[start + 1..end - 1]
		} else if value_kind == .object {
		} else if value_kind == .array {
		} else {
			return decoder.json[start..end]
		}
		return ''
	} $else $if T is $int {
		if value_kind == .string_ {
			return decoder.json[start + 1..end - 1].int()
		} else if value_kind == .object {
		} else if value_kind == .array {
		} else {
			return decoder.json[start..end].int()
		}
	}
	return T{}
}

// decode_struct decodes a struct from the JSON nodes.
fn (mut decoder Decoder) decode_struct[T](nodes []Node, value &T) {
	$for field in T.fields {
		for i := 0; i < nodes.len; i++ {
			mut node := nodes[i]

			if node.key_len == field.name.len {
				// This `vmemcmp` compares the name of a key in a JSON with a given struct field.
				if unsafe {
					vmemcmp(decoder.json.str + node.key_pos, field.name.str, field.name.len) == 0
				} {
					start := (node.key_pos + node.key_len) + 3
					mut end := start
					for decoder.json[end] != `,` && decoder.json[end] != `}` {
						end++
					}
					value_kind := get_value_kind(decoder.json[start])
					$if field.indirections != 0 {
						// REVIEW Needs clone?
						$if field.indirections == 1 {
							// TODO
							// unsafe {
							// 	value.$(field.name) = &(decoder.json[start + 1..end - 1])
							// }
						} $else $if field.indirections == 2 {
							// TODO
							// unsafe {
							// 	value.$(field.name) = &&(decoder.json[start + 1..end - 1])
							// }
						} $else $if field.indirections == 3 {
							// TODO
							// unsafe {
							// 	value.$(field.name) = &&&(decoder.json[start + 1..end - 1])
							// }
						}
					} $else $if field.typ is $option {
						value.$(field.name) = decoder.decode_optional_value_in_actual_node(node,
							value.$(field.name))
					} $else $if field.typ is $sumtype {
						// dump(value.$(field.name))

						workaround := value.$(field.name)
						// z := value.$(field.name)

						$for v in workaround.variants {
							$if v.typ is string {
								if value_kind == .string_ {
									// value.$(field.name) = decoder.json[start + 1..end - 1]
								} else {
									// value.$(field.name) = decoder.json[start..end]
								}
							} $else $if v.typ in [$int, $float] {
								$if v.typ is u32 {
									value.$(field.name) = decoder.json[start..end].u32()
								} $else $if v.typ is u32 {
								}

								$if v.typ is i8 {
									value.$(field.name) = decoder.json[start..end].i8()
								} $else $if v.typ is i16 {
									value.$(field.name) = decoder.json[start..end].i16()
								} $else $if v.typ is i32 {
									value.$(field.name) = decoder.json[start..end].i32()
								} $else $if v.typ is int {
									value.$(field.name) = decoder.json[start..end].int()
								} $else $if v.typ is i64 {
									value.$(field.name) = decoder.json[start..end].i64()
								} $else $if v.typ is u8 {
									value.$(field.name) = decoder.json[start..end].u8()
								} $else $if v.typ is u16 {
									value.$(field.name) = decoder.json[start..end].u16()
								} $else $if v.typ is u32 {
									value.$(field.name) = decoder.json[start..end].u32()
								} $else $if v.typ is u64 {
									value.$(field.name) = decoder.json[start..end].u64()
								} $else $if v.typ is f32 {
									value.$(field.name) = decoder.json[start..end].f32()
								} $else $if v.typ is f64 {
									value.$(field.name) = decoder.json[start..end].f64()
								}
							} $else $if v.typ is bool {
								if decoder.json[start] == `t` {
									value.$(field.name) = true
								} else if decoder.json[start] == `f` {
									value.$(field.name) = false
								}
							} $else $if v.typ is time.Time {
								if value_kind == .string_ {
									value.$(field.name) = time.parse(decoder.json[start + 1..end - 1]) or {
										time.Time{}
									}
								}
							} $else $if v.typ is $struct {
								if node.children != none {
									// FIXME
									// decoder.decode_value(node.children or {
									// 	panic('It will never happens')
									// }, value.$(field.name))
								}
							} $else $if v.typ is $array {
								if value_kind == .array {
									// TODO
								}
							} $else $if v.typ is $map {
								if value_kind == .object {
									// TODO
								}
							} $else $if T is $enum {
							} $else {
								eprintln('not supported')
							}
						}
						if value_kind == .string_ {
							// value.$(field.name) = decoder.json[start + 1..end - 1]
						} else if decoder.json[start] == `t` {
							value.$(field.name) = true
						} else if decoder.json[start] == `f` {
							value.$(field.name) = false
						} else if value_kind == .object {
						} else if value_kind == .array {
						} else if value_kind == .number {
							// value.$(field.name) = decoder.json[start..end].int()
						} else {
						}
					} $else $if field.typ is string {
						value.$(field.name) = if value_kind == .string_ {
							decoder.json[start + 1..end - 1]
						} else {
							decoder.json[start..end]
						}
					} $else $if field.typ in [$int, $float] {
						$if field.typ is i8 {
							value.$(field.name) = decoder.json[start..end].i8()
						} $else $if field.typ is i16 {
							value.$(field.name) = decoder.json[start..end].i16()
						} $else $if field.typ is i32 {
							value.$(field.name) = decoder.json[start..end].i32()
						} $else $if field.typ is int {
							value.$(field.name) = decoder.json[start..end].int()
						} $else $if field.typ is i64 {
							value.$(field.name) = decoder.json[start..end].i64()
						} $else $if field.typ is u8 {
							value.$(field.name) = decoder.json[start..end].u8()
						} $else $if field.typ is u16 {
							value.$(field.name) = decoder.json[start..end].u16()
						} $else $if field.typ is u32 {
							value.$(field.name) = decoder.json[start..end].u32()
						} $else $if field.typ is u64 {
							value.$(field.name) = decoder.json[start..end].u64()
						} $else $if field.typ is f32 {
							value.$(field.name) = decoder.json[start..end].f32()
						} $else $if field.typ is f64 {
							value.$(field.name) = decoder.json[start..end].f64()
						}
					} $else $if field.typ is bool {
						value.$(field.name) = decoder.json[start] == `t`
					} $else $if field.typ is time.Time {
						if value_kind == .string_ {
							value.$(field.name) = time.parse_rfc3339(decoder.json[start + 1..end - 1]) or {
								time.Time{}
							}
						}
					} $else $if field.typ is $struct {
						if node.children != none {
							decoder.decode_value(node.children or { panic('It will never happen') },
								value.$(field.name))
						}
					} $else $if field.typ is $array {
						if value_kind == .array {
							// TODO
						}
					} $else $if field.typ is $map {
						if value_kind == .object && node.children != none {
							decoder.decode_map(node.children or { panic('It will never happen') }, mut
								value.$(field.name))
						}
					} $else $if field.typ is $enum {
						value.$(field.name) = decoder.json[start..end].int()
					} $else $if field.typ is $alias {
						$if field.unaliased_typ is string {
							if value_kind == .string_ {
								value.$(field.name) = decoder.json[start + 1..end - 1]
							}
						} $else $if field.unaliased_typ is time.Time {
						} $else $if field.unaliased_typ is bool {
						} $else $if field.unaliased_typ in [$float, $int] {
							$if field.unaliased_typ is i8 {
								value.$(field.name) = decoder.json[start..end].i8()
							} $else $if field.unaliased_typ is i16 {
								value.$(field.name) = decoder.json[start..end].i16()
							} $else $if field.unaliased_typ is i32 {
								value.$(field.name) = decoder.json[start..end].i32()
							} $else $if field.unaliased_typ is int {
								value.$(field.name) = decoder.json[start..end].int()
							} $else $if field.unaliased_typ is i64 {
								value.$(field.name) = decoder.json[start..end].i64()
							} $else $if field.unaliased_typ is u8 {
								value.$(field.name) = decoder.json[start..end].u8()
							} $else $if field.unaliased_typ is u16 {
								value.$(field.name) = decoder.json[start..end].u16()
							} $else $if field.unaliased_typ is u32 {
								value.$(field.name) = decoder.json[start..end].u32()
							} $else $if field.unaliased_typ is u64 {
								value.$(field.name) = decoder.json[start..end].u64()
							} $else $if field.unaliased_typ is f32 {
								value.$(field.name) = decoder.json[start..end].f32()
							} $else $if field.unaliased_typ is f64 {
								value.$(field.name) = decoder.json[start..end].f64()
							}
						} $else $if field.unaliased_typ is $array {
							// TODO
						} $else $if field.unaliased_typ is $struct {
						} $else $if field.unaliased_typ is $enum {
							// TODO
						} $else $if field.unaliased_typ is $sumtype {
							// TODO
						} $else {
							eprintln('the alias ${field.unaliased_typ} cannot be encoded')
						}
					} $else {
						eprintln('not supported')
					}
					break
				}
			}
		}
	}
}

// decode_map decodes a map from the JSON nodes.
fn (mut decoder Decoder) decode_map[T](nodes []Node, mut val T) {
	for i := 0; i < nodes.len; i++ {
		mut node := nodes[i]

		start := (node.key_pos + node.key_len) + 3
		mut end := start
		for decoder.json[end] != `,` && decoder.json[end] != `}` {
			end++
		}
		value_kind := get_value_kind(decoder.json[start])
		val[decoder.json[node.key_pos..node.key_pos + node.key_len]] = if value_kind == .string_ {
			decoder.json[start + 1..end - 1]
		} else {
			decoder.json[start..end]
		}
	}
}

// fulfill_nodes fills the nodes from the JSON string.
fn (mut decoder Decoder) fulfill_nodes(mut nodes []Node) {
	mut inside_string := false
	mut inside_key := false
	mut actual_key_len := 0

	for decoder.idx < decoder.json.len {
		letter := decoder.json[decoder.idx]
		match letter {
			` ` {
				if !inside_string {
				}
			}
			`\"` {
				if decoder.json[decoder.idx - 1] == `{` || decoder.json[decoder.idx - 2] == `,` {
					inside_key = true
				} else if decoder.json[decoder.idx + 1] == `:` {
					if decoder.json[decoder.idx + 3] == `{` {
						mut children := []Node{}
						key_pos := decoder.idx - actual_key_len
						key_len := actual_key_len

						decoder.idx += 3
						decoder.fulfill_nodes(mut children)

						nodes << Node{
							key_pos:  key_pos
							key_len:  key_len
							children: children
						}
					} else {
						nodes << Node{
							key_pos: decoder.idx - actual_key_len
							key_len: actual_key_len
						}
					}
					inside_key = false
				}
				inside_string = !inside_string
				decoder.idx++
				continue
			}
			`:` {
				actual_key_len = 0
			}
			`,`, `{`, `}`, `[`, `]` {}
			else {}
		}
		if inside_key {
			actual_key_len++
		}
		decoder.idx++
	}
}
