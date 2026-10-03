module json2

import time

struct SumtypeTimeValue {
	typ   string @[json: '_type'; required]
	value i64    @[required]
}

fn sumtype_variant_name(type_name string) string {
	return type_name.all_after_last('.')
}

// struct_variant_tag returns the `_type` of a struct sum type variant of type `T`: the
// struct's own name, also for an alias of it (`Foo` for `type Alias = Foo`), like the
// removed `json` module wrote it.
fn struct_variant_tag[T]() string {
	return sumtype_variant_name(typeof($zero(T.unaliased_typ)).name)
}

// option_payload_tag returns the `_type` of an option variant of a struct or a time.
fn option_payload_tag[P](_ ?P) string {
	return struct_variant_tag[P]()
}

fn (mut decoder Decoder) get_decoded_sumtype_workaround[T](initialized_sumtype T) !T {
	$if initialized_sumtype is $sumtype || (T is $alias && T.unaliased_typ is $sumtype) {
		resolved_sumtype := initialized_sumtype
		// `is` does not tell an alias variant from its base type (`MyString | string`
		// matches both), so prefer the variant with the exact type name.
		variant_name := initialized_sumtype.type_name()
		mut has_exact_variant := false
		$for v in T.variants {
			if initialized_sumtype is v && variant_name == typeof(v.typ).name {
				has_exact_variant = true
			}
		}
		$for v in T.variants {
			if initialized_sumtype is v
				&& (!has_exact_variant || variant_name == typeof(v.typ).name) {
				$if initialized_sumtype is time.Time {
					mut val := $zero(v.typ)
					decoder.decode_sumtype_time(mut val)!
					return T(val)
				} $else $if initialized_sumtype !is $option {
					mut val := $zero(v.typ)
					decoder.decode_value(mut val)!
					return T(val)
				} $else {
					if decoder.current_node.value.value_kind == .null {
						decoder.current_node = decoder.current_node.next
						return resolved_sumtype
					} else {
						// The payload of an option variant, like in the removed `json` module.
						mut option_value := $zero(v.typ)
						option_value = decoder.decode_option_payload(option_value)!
						return T(option_value)
					}
				}
			}
		}
	}
	decoder.decode_error('could not decode resolved sumtype (should not happen)')!
	return initialized_sumtype // suppress compiler error
}

fn (mut decoder Decoder) check_element_type_valid[T](element T, current_node &DecodeNode[ValueInfo]) bool {
	if current_node == unsafe { nil } {
		$if element is $array || element is $map {
			return false
		}
		return true
	}

	$if element is $sumtype { // this will always match the first sumtype array/map
		return true
	}
	$if element is $option {
		// A `none` element is `null`, or `{}` as the removed module wrote it; a set one
		// has the shape of its payload.
		if current_node.value.value_kind == .null || decoder.is_empty_object(current_node) {
			return true
		}
		return decoder.check_option_element_valid(element, current_node)
	}

	match current_node.value.value_kind {
		.string {
			$if element is string {
				return true
			} $else $if element is time.Time {
				return true
			} $else $if element is StringDecoder {
				return true
			}
		}
		.number {
			$if element is $float {
				return true
			} $else $if element is $int {
				return true
			} $else $if element is $enum {
				return true
			} $else $if element is NumberDecoder {
				return true
			}
		}
		.boolean {
			$if element is bool {
				return true
			} $else $if element is BooleanDecoder {
				return true
			}
		}
		.null {
			$if element is $option {
				return true
			} $else $if element is NullDecoder {
				return true
			}
		}
		.array {
			$if element is $array {
				return decoder.check_array_type_valid(element, current_node.next)
			}
		}
		.object {
			$if element is $map {
				if current_node.next != unsafe { nil } {
					return decoder.check_map_type_valid(element, current_node.next.next)
				} else {
					return decoder.check_map_type_valid(element, unsafe { nil })
				}
			} $else $if element is $struct {
				return decoder.check_struct_type_valid(element, current_node)
			}
		}
	}

	return false
}

fn (mut decoder Decoder) check_option_element_valid[P](_ ?P, current_node &DecodeNode[ValueInfo]) bool {
	return decoder.check_element_type_valid(P{}, current_node)
}

// is_empty_object reports whether `node` is an object without members (`{}`).
fn (decoder &Decoder) is_empty_object(node &DecodeNode[ValueInfo]) bool {
	if node.value.value_kind != .object {
		return false
	}
	end := node.value.position + node.value.length
	return node.next == unsafe { nil } || node.next.value.position >= end
}

fn get_array_element_type[T](_arr []T) T {
	return T{}
}

fn (mut decoder Decoder) check_array_type_valid[T](arr []T, current_node &DecodeNode[ValueInfo]) bool {
	element := get_array_element_type(arr)
	return decoder.check_element_type_valid(element, current_node)
}

fn (mut decoder Decoder) get_array_type_workaround[T](initialized_sumtype T) bool {
	$if initialized_sumtype is $sumtype || (T is $alias && T.unaliased_typ is $sumtype) {
		$for v in T.variants {
			if initialized_sumtype is v {
				$if initialized_sumtype is $array {
					return decoder.check_element_type_valid(initialized_sumtype,
						decoder.current_node)
				}
			}
		}
	}
	return false
}

fn get_map_element_type[U, V](_m map[U]V) V {
	return V{}
}

fn (mut decoder Decoder) check_map_type_valid[T](m T, current_node &DecodeNode[ValueInfo]) bool {
	element := get_map_element_type(m)
	return decoder.check_element_type_valid(element, current_node)
}

fn (mut decoder Decoder) check_map_empty_valid[T](m T) bool {
	element := get_map_element_type(m)
	return decoder.check_element_type_valid(element, current_node)
}

fn (mut decoder Decoder) get_map_type_workaround[T](initialized_sumtype T) bool {
	$if initialized_sumtype is $sumtype || (T is $alias && T.unaliased_typ is $sumtype) {
		$for v in T.variants {
			if initialized_sumtype is v {
				$if initialized_sumtype is $map {
					val := $zero(v.typ)
					if decoder.current_node.next != unsafe { nil } {
						return decoder.check_map_type_valid(val, decoder.current_node.next.next)
					} else {
						return decoder.check_map_type_valid(val, unsafe { nil })
					}
				}
			}
		}
	}
	return false
}

@[markused]
fn (mut decoder Decoder) get_sumtype_type_field_node(current_node &DecodeNode[ValueInfo]) &DecodeNode[ValueInfo] {
	if current_node == unsafe { nil } || current_node.value.value_kind != .object {
		return unsafe { nil }
	}
	// Look at the object's own keys only: a nested value (such as a field holding
	// another sum type, which is encoded before the outer `_type`) can have a
	// `_type` of its own.
	object_end := current_node.value.position + current_node.value.length
	mut key_node := current_node.next
	for key_node != unsafe { nil } && key_node.value.position < object_end {
		value_node := key_node.next
		if value_node == unsafe { nil } {
			break
		}
		// A key spelled with escapes (`"_\u0074ype"`) is `_type` after unescaping.
		if decoder.json_key_matches(key_node.value, '_type') or { false } {
			return value_node
		}
		// Skip the value, with everything nested in it.
		value_end := value_node.value.position + value_node.value.length
		mut next_node := value_node.next
		for next_node != unsafe { nil } && next_node.value.position < value_end {
			next_node = next_node.next
		}
		key_node = next_node
	}
	return unsafe { nil }
}

@[markused]
fn (mut decoder Decoder) sumtype_type_field_matches(type_field_node &DecodeNode[ValueInfo], expected string) bool {
	if type_field_node == unsafe { nil } {
		return false
	}
	value_info := type_field_node.value
	if value_info.value_kind != .string {
		return false
	}
	body := decoder.json[value_info.position + 1..value_info.position + value_info.length - 1]
	if body.index_u8(`\\`) != -1 {
		// An escaped discriminator (`"Hum\u0061n"`) names the variant after unescaping.
		decoded := decoder.decode_string_value(value_info) or { return false }
		return decoded == expected
	}
	return body == expected
}

fn (mut decoder Decoder) check_sumtype_type_valid[T](value T, current_node &DecodeNode[ValueInfo]) bool {
	type_field_node := decoder.get_sumtype_type_field_node(current_node)
	// An alias of a struct is tagged with its own name, or with the struct's name.
	return decoder.sumtype_type_field_matches(type_field_node,
		sumtype_variant_name(typeof(value).name))
		|| decoder.sumtype_type_field_matches(type_field_node, struct_variant_tag[T]())
}

fn (mut decoder Decoder) check_struct_type_valid[T](s T, current_node &DecodeNode[ValueInfo]) bool {
	return decoder.check_sumtype_type_valid(s, current_node)
}

// resolve_sumtype_from_type_field selects the struct (or time.Time) variant named by
// the object's `_type` field. The name is matched before anything is constructed:
// building a variant runs its field defaults, which may have side effects, so only
// the selected variant may be built.
fn (mut decoder Decoder) resolve_sumtype_from_type_field[T](mut val T) !bool {
	type_field_node := decoder.get_sumtype_type_field_node(decoder.current_node)
	if type_field_node == unsafe { nil } {
		return false
	}
	mut has_discriminated_variant := false
	// A variant is tagged with its payload's name, like in the removed `json` module:
	// the struct's name for an alias of a struct (`Foo` for `type Alias = Foo`), and
	// `Time` for a time or an alias of it. The variant's own name is accepted too, and
	// is tried first, so `Foo | Alias` tells both apart.
	for base_names in [false, true] {
		$for v in T.variants {
			$if v.typ is $option {
				// An option of a struct or a time (`?Foo`, `?time.Time`) is tagged with
				// its payload's name.
				option_value := $zero(v.typ)
				if option_payload_fit(option_value, .object) > 0 {
					has_discriminated_variant = true
					name := if base_names {
						option_payload_tag(option_value)
					} else {
						sumtype_variant_name(typeof(v.typ).name.trim_left('?'))
					}
					if decoder.sumtype_type_field_matches(type_field_node, name) {
						val = T(v)
						return true
					}
				}
			} $else $if v.typ is $struct {
				has_discriminated_variant = true
				name := if base_names {
					sumtype_variant_name(typeof(v.typ.unaliased_typ).name)
				} else {
					sumtype_variant_name(typeof(v.typ).name)
				}
				if decoder.sumtype_type_field_matches(type_field_node, name) {
					val = T(v)
					return true
				}
			}
		}
	}
	if !has_discriminated_variant {
		return false
	}
	decoder.decode_error('could not resolve sumtype `${T.name}` from "_type" field')!
	return false
}

@[markused]
fn (mut decoder Decoder) decode_sumtype_time(mut val time.Time) ! {
	mut wrapper := SumtypeTimeValue{
		typ:   ''
		value: 0
	}
	decoder.decode_value(mut wrapper)!
	val = time.unix(wrapper.value)
}

fn (mut decoder Decoder) init_sumtype_by_value_kind[T](mut val T, value_info ValueInfo) ! {
	mut failed_struct := false
	mut struct_variant_count := 0

	// For a string, number or boolean, a variant of the plain type wins over an alias
	// of it (`MyString | string` selects `string`), like in the removed `json` module:
	// aliases are only tried after all other variants.
	match value_info.value_kind {
		.string {
			for aliases in [false, true] {
				$for v in T.variants {
					mut is_alias := false
					$if v.typ is $alias {
						is_alias = true
					}
					if is_alias == aliases {
						$if v.typ is string {
							val = T(v)
							return
						} $else $if v.typ is time.Time {
							val = T(v)
							return
						} $else $if v.typ is StringDecoder {
							val = T(v)
							return
						}
					}
				}
			}
		}
		.number {
			for aliases in [false, true] {
				$for v in T.variants {
					mut is_alias := false
					$if v.typ is $alias {
						is_alias = true
					}
					if is_alias == aliases {
						$if v.typ is $float {
							val = T(v)
							return
						} $else $if v.typ is $int {
							val = T(v)
							return
						} $else $if v.typ is $enum {
							val = T(v)
							return
						} $else $if v.typ is NumberDecoder {
							val = T(v)
							return
						}
					}
				}
			}
		}
		.boolean {
			for aliases in [false, true] {
				$for v in T.variants {
					mut is_alias := false
					$if v.typ is $alias {
						is_alias = true
					}
					if is_alias == aliases {
						$if v.typ is bool {
							val = T(v)
							return
						} $else $if v.typ is BooleanDecoder {
							val = T(v)
							return
						}
					}
				}
			}
		}
		.null {
			$for v in T.variants {
				$if v.typ is $option {
					val = T(v)
					return
				} $else $if v.typ is NullDecoder {
					val = T(v)
					return
				}
			}
		}
		.array {
			$for v in T.variants {
				$if v.typ is $array {
					val = T(v)

					if decoder.get_array_type_workaround(val) {
						return
					}
				}
			}
		}
		.object {
			if decoder.resolve_sumtype_from_type_field(mut val)! {
				return
			}
			$for v in T.variants {
				$if v.typ is $map {
					val = T(v)

					if decoder.get_map_type_workaround(val) {
						return
					}
				} $else $if v.typ is $struct {
					// Without a `_type` field no struct variant matches by name; the
					// only one that can be selected is built below.
					struct_variant_count++
					failed_struct = true
				}
			}
		}
	}

	if failed_struct {
		// If there is only one struct variant and no explicit `_type` key,
		// the object shape is already unambiguous.
		if struct_variant_count == 1
			&& decoder.get_sumtype_type_field_node(decoder.current_node) == unsafe { nil } {
			$for v in T.variants {
				$if v.typ is $struct {
					val = T(v)
				}
			}
			return
		}
		decoder.decode_error('could not resolve sumtype `${T.name}`, missing "_type" field?')!
	}
	// A value no other variant takes goes to an option variant, whose payload has to
	// decode it (`5` for `?int | string`), like in the removed `json` module: first one
	// whose payload takes the JSON value as it is (`?string` for a string), then one
	// that converts it (`?rune` for a string), then any.
	if value_info.value_kind != .null {
		for min_fit in [2, 1, 0] {
			$for v in T.variants {
				$if v.typ is $option {
					if option_payload_fit($zero(v.typ), value_info.value_kind) >= min_fit {
						val = T(v)
						return
					}
				}
			}
		}
	}

	decoder.decode_error('could not resolve sumtype `${T.name}`, got ${value_info.value_kind}.')!
}

// option_payload_fit reports how the payload of an option (`?int`) takes a JSON value
// of `kind`: 2 when it is of that kind (`?string` for a string), 1 when it converts it
// (`?rune` or `?time.Time` for a string), and 0 when it does not take it. Aliases are
// unwrapped, so `?Text` takes a string for `type Text = string`.
fn option_payload_fit[P](_ ?P, kind ValueKind) int {
	$if P.unaliased_typ is time.Time {
		return if kind == .string || kind == .object { 1 } else { 0 }
	} $else $if P.unaliased_typ is string {
		return if kind == .string { 2 } else { 0 }
	} $else $if P.unaliased_typ is bool {
		return if kind == .boolean { 2 } else { 0 }
	} $else $if P.unaliased_typ is rune {
		// A rune is written as a string, and a number is taken as its code point.
		return if kind == .string || kind == .number { 1 } else { 0 }
	} $else $if P.unaliased_typ is $int || P.unaliased_typ is $float {
		return if kind == .number { 2 } else { 0 }
	} $else $if P.unaliased_typ is $enum {
		return if kind == .string || kind == .number { 1 } else { 0 }
	} $else $if P.unaliased_typ is $array || P.unaliased_typ is $array_fixed {
		return if kind == .array { 2 } else { 0 }
	} $else {
		return if kind == .object { 2 } else { 0 }
	}
}

fn (mut decoder Decoder) decode_sumtype[T](mut val T) ! {
	value_info := decoder.current_node.value

	decoder.init_sumtype_by_value_kind(mut val, value_info)!

	val = decoder.get_decoded_sumtype_workaround(val)!
}
