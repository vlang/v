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
					if decoder.current_value().value_kind == .null {
						decoder.current_idx++
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

// check_element_type_valid reports whether the value at `value_idx` in values_info
// has the shape of `element`.
fn (mut decoder Decoder) check_element_type_valid[T](element T, value_idx int) bool {
	if !decoder.has_value(value_idx) {
		$if element is $array || element is $map {
			return false
		}
		return true
	}
	value_kind := decoder.values_info[value_idx].value_kind

	$if element is $sumtype { // this will always match the first sumtype array/map
		return true
	}
	$if element is $option {
		// A `none` element is `null`, or `{}` as the removed module wrote it; a set one
		// has the shape of its payload.
		if value_kind == .null || decoder.is_empty_object(value_idx) {
			return true
		}
		return decoder.check_option_element_valid(element, value_idx)
	}

	match value_kind {
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
				return decoder.check_array_type_valid(element, value_idx + 1)
			}
		}
		.object {
			$if element is $map {
				// The first value of the object, after its first key.
				return decoder.check_map_type_valid(element, value_idx + 2)
			} $else $if element is $struct {
				return decoder.check_struct_type_valid(element, value_idx)
			}
		}
	}

	return false
}

fn (mut decoder Decoder) check_option_element_valid[P](_ ?P, value_idx int) bool {
	return decoder.check_element_type_valid(P{}, value_idx)
}

// is_empty_object reports whether the value at `value_idx` is an object without
// members (`{}`).
fn (decoder &Decoder) is_empty_object(value_idx int) bool {
	value_info := decoder.values_info[value_idx]
	if value_info.value_kind != .object {
		return false
	}
	end := value_info.position + value_info.length
	return !decoder.has_value(value_idx + 1) || decoder.values_info[value_idx + 1].position >= end
}

fn get_array_element_type[T](_arr []T) T {
	return T{}
}

fn (mut decoder Decoder) check_array_type_valid[T](arr []T, value_idx int) bool {
	element := get_array_element_type(arr)
	return decoder.check_element_type_valid(element, value_idx)
}

fn (mut decoder Decoder) get_array_type_workaround[T](initialized_sumtype T) bool {
	$if initialized_sumtype is $sumtype || (T is $alias && T.unaliased_typ is $sumtype) {
		$for v in T.variants {
			if initialized_sumtype is v {
				$if initialized_sumtype is $array {
					return decoder.check_element_type_valid(initialized_sumtype,
						decoder.current_idx)
				}
			}
		}
	}
	return false
}

fn get_map_element_type[U, V](_m map[U]V) V {
	return V{}
}

fn (mut decoder Decoder) check_map_type_valid[T](m T, value_idx int) bool {
	element := get_map_element_type(m)
	return decoder.check_element_type_valid(element, value_idx)
}

fn (mut decoder Decoder) check_map_empty_valid[T](m T) bool {
	element := get_map_element_type(m)
	return decoder.check_element_type_valid(element, no_value_idx)
}

fn (mut decoder Decoder) get_map_type_workaround[T](initialized_sumtype T) bool {
	$if initialized_sumtype is $sumtype || (T is $alias && T.unaliased_typ is $sumtype) {
		$for v in T.variants {
			if initialized_sumtype is v {
				$if initialized_sumtype is $map {
					val := $zero(v.typ)
					// The first value of the object, after its first key.
					return decoder.check_map_type_valid(val, decoder.current_idx + 2)
				}
			}
		}
	}
	return false
}

// get_sumtype_type_field_idx returns the index in values_info of the value of the
// `_type` key of the object at `value_idx`, or no_value_idx without such a key.
@[direct_array_access; markused]
fn (mut decoder Decoder) get_sumtype_type_field_idx(value_idx int) int {
	if !decoder.has_value(value_idx) || decoder.values_info[value_idx].value_kind != .object {
		return no_value_idx
	}
	// Look at the object's own keys only: a nested value (such as a field holding
	// another sum type, which is encoded before the outer `_type`) can have a
	// `_type` of its own.
	object_info := decoder.values_info[value_idx]
	object_end := object_info.position + object_info.length
	values_len := decoder.values_info.len
	mut key_idx := value_idx + 1
	for key_idx < values_len && decoder.values_info[key_idx].position < object_end {
		key_value_idx := key_idx + 1
		if key_value_idx >= values_len {
			break
		}
		// A key spelled with escapes (`"_\u0074ype"`) is `_type` after unescaping.
		if decoder.json_key_matches(decoder.values_info[key_idx], '_type') or { false } {
			return key_value_idx
		}
		// Skip the value, with everything nested in it.
		value_info := decoder.values_info[key_value_idx]
		value_end := value_info.position + value_info.length
		key_idx = key_value_idx + 1
		for key_idx < values_len && decoder.values_info[key_idx].position < value_end {
			key_idx++
		}
	}
	return no_value_idx
}

@[markused]
fn (mut decoder Decoder) sumtype_type_field_matches(type_field_idx int, expected string) bool {
	if !decoder.has_value(type_field_idx) {
		return false
	}
	value_info := decoder.values_info[type_field_idx]
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

fn (mut decoder Decoder) check_sumtype_type_valid[T](value T, value_idx int) bool {
	type_field_idx := decoder.get_sumtype_type_field_idx(value_idx)
	// An alias of a struct is tagged with its own name, or with the struct's name.
	return decoder.sumtype_type_field_matches(type_field_idx,
		sumtype_variant_name(typeof(value).name))
		|| decoder.sumtype_type_field_matches(type_field_idx, struct_variant_tag[T]())
}

fn (mut decoder Decoder) check_struct_type_valid[T](s T, value_idx int) bool {
	return decoder.check_sumtype_type_valid(s, value_idx)
}

// resolve_sumtype_from_type_field selects the struct (or time.Time) variant named by
// the object's `_type` field. The name is matched before anything is constructed:
// building a variant runs its field defaults, which may have side effects, so only
// the selected variant may be built.
fn (mut decoder Decoder) resolve_sumtype_from_type_field[T](mut val T) !bool {
	type_field_idx := decoder.get_sumtype_type_field_idx(decoder.current_idx)
	if type_field_idx == no_value_idx {
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
					if decoder.sumtype_type_field_matches(type_field_idx, name) {
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
				if decoder.sumtype_type_field_matches(type_field_idx, name) {
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
			&& decoder.get_sumtype_type_field_idx(decoder.current_idx) == no_value_idx {
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
	value_info := decoder.current_value()

	decoder.init_sumtype_by_value_kind(mut val, value_info)!

	val = decoder.get_decoded_sumtype_workaround(val)!
}
