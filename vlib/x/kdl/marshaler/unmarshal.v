module marshaler

import x.kdl.document
import x.kdl.parser

pub interface Unmarshaler {
mut:
	from_kdl(data string) !
}

pub struct DecodeOpts {
}

pub fn decode[T](data string) !T {
	return decode_opts[T](data, DecodeOpts{})
}

pub fn decode_opts[T](data string, opts DecodeOpts) !T {
	// opts currently has no fields; parse options will be wired when DecodeOpts gains them
	doc := parser.parse(data)!
	mut result := T{}
	$if T is Unmarshaler {
		result.from_kdl(data)!
		return result
	}
	$if T is $struct {
		if doc.nodes.len > 0 {
			decode_struct(mut result, doc.nodes[0], opts)
		}
		return result
	}
	return result
}

fn decode_struct[T](mut val T, node document.Node, opts DecodeOpts) {
	_ := opts
	mut args := []document.Value{}
	mut props := map[string]document.Value{}
	for e in node.entries {
		match e {
			document.Argument { args << e.value }
			document.Property { props[e.key] = e.value }
		}
	}

	$for field in T.fields {
		tag := kdl_tag(field.attrs)
		_, attrs := parse_kdl_tag(tag)
		if 'arg' in attrs {
			if args.len > 0 {
				decode_value_to_field(mut val, field.name, args[0])
				args = unsafe { args[1..] }
			}
		}
	}
	$for field in T.fields {
		tag := kdl_tag(field.attrs)
		_, attrs := parse_kdl_tag(tag)
		if 'args' in attrs {
			collect_args_to_slice(mut val, field.name, args)
		}
	}
	for key, value in props {
		$for field in T.fields {
			tag := kdl_tag(field.attrs)
			tag_name, attrs := parse_kdl_tag(tag)
			is_special := 'arg' in attrs || 'args' in attrs || 'children' in attrs
				|| 'child' in attrs || 'props' in attrs
			if !is_special {
				fname := if tag_name != '' { tag_name } else { field.name }
				if key == fname {
					decode_value_to_field(mut val, field.name, value)
				}
			}
		}
	}

	$for field in T.fields {
		tag := kdl_tag(field.attrs)
		_, attrs := parse_kdl_tag(tag)
		if 'props' in attrs {
			for key, value in props {
				decode_prop_to_map(mut val, field.name, key, value)
			}
		}
	}
	$for field in T.fields {
		tag := kdl_tag(field.attrs)
		tag_name, attrs := parse_kdl_tag(tag)
		if 'children' in attrs || 'child' in attrs {
			for child in node.children {
				fname := if tag_name != '' { tag_name } else { field.name }
				if 'children' in attrs || child.name == fname {
					decode_child_to_field(mut val, field.name, child, opts)
				}
			}
		}
	}
}

fn decode_value_to_field[T](mut val T, field_name string, v document.Value) {
	$for field in T.fields {
		if field.name == field_name {
			$if field.typ is string { val.$(field.name) = from_value_string(v) }
			$if field.typ is int { val.$(field.name) = from_value_int(v) }
			$if field.typ is bool { val.$(field.name) = from_value_bool(v) }
			$if field.typ is f64 { val.$(field.name) = from_value_f64(v) }
			$if field.typ is i64 { val.$(field.name) = from_value_i64(v) }
			$if field.typ is f32 { val.$(field.name) = f32(from_value_f64(v)) }
			$if field.typ is u64 { val.$(field.name) = u64(from_value_i64(v)) }
			$if field.typ is i16 { val.$(field.name) = i16(from_value_int(v)) }
			$if field.typ is u16 { val.$(field.name) = u16(from_value_int(v)) }
			$if field.typ is i8 { val.$(field.name) = i8(from_value_int(v)) }
			$if field.typ is u8 { val.$(field.name) = u8(from_value_int(v)) }
			$if field.typ is u32 { val.$(field.name) = u32(from_value_int(v)) }
		}
	}
}

fn collect_args_to_slice[T](mut val T, field_name string, args []document.Value) {
	$for field in T.fields {
		if field.name == field_name {
			$if field.typ is $array {
				mut arr := val.$(field.name)
				for a in args {
					$if field.typ is []string { arr << from_value_string(a) }
					$if field.typ is []int { arr << from_value_int(a) }
					$if field.typ is []f64 { arr << from_value_f64(a) }
					$if field.typ is []bool { arr << from_value_bool(a) }
					$if field.typ is []i64 { arr << from_value_i64(a) }
				}
				val.$(field.name) = arr
			}
		}
	}
}

fn decode_prop_to_map[T](mut val T, field_name string, key string, v document.Value) {
	$for field in T.fields {
		if field.name == field_name {
			$if field.typ is $map {
				mut m := val.$(field.name)
				insert_map_entry(mut m, key, v)
				val.$(field.name) = m
			}
		}
	}
}

fn decode_child_to_field[T](mut val T, field_name string, child document.Node, opts DecodeOpts) {
	$for field in T.fields {
		if field.name == field_name {
			$if field.typ is $struct {
				mut sub := val.$(field.name)
				decode_struct(mut sub, child, opts)
				val.$(field.name) = sub
			}
			$if field.typ is $map {
				mut m := val.$(field.name)
				if child.entries.len > 0 {
					match child.entries[0] {
						document.Argument {
							insert_map_entry(mut m, child.name, child.entries[0].value)
						}
						document.Property {}
					}
				}
				val.$(field.name) = m
			}
		}
	}
}

fn insert_map_entry[T](mut m T, key string, v document.Value) {
	$if T is map[string]string { m[key] = from_value_string(v) }
	$if T is map[string]int { m[key] = from_value_int(v) }
	$if T is map[string]bool { m[key] = from_value_bool(v) }
	$if T is map[string]f64 { m[key] = from_value_f64(v) }
	$if T is map[string]i64 { m[key] = from_value_i64(v) }
}

fn from_value_string(v document.Value) string {
	match v {
		document.StringVal { return v.value }
		document.IntVal { return v.value.str() }
		document.FloatVal { return v.value.str() }
		document.BoolVal { return if v.value { 'true' } else { 'false' } }
		document.NullVal { return '' }
	}

	return ''
}

fn from_value_int(v document.Value) int {
	match v {
		document.IntVal { return int(v.value) }
		document.FloatVal { return int(v.value) }
		document.StringVal { return v.value.int() }
		document.BoolVal { return if v.value { 1 } else { 0 } }
		document.NullVal { return 0 }
	}

	return 0
}

fn from_value_bool(v document.Value) bool {
	match v {
		document.BoolVal { return v.value }
		document.IntVal { return v.value != 0 }
		document.FloatVal { return v.value != 0.0 }
		document.StringVal { return v.value.to_lower() in ['true', 'yes', 'on', '1'] }
		document.NullVal { return false }
	}

	return false
}

fn from_value_f64(v document.Value) f64 {
	match v {
		document.FloatVal { return v.value }
		document.IntVal { return f64(v.value) }
		document.StringVal { return v.value.f64() }
		document.BoolVal { return if v.value { 1.0 } else { 0.0 } }
		document.NullVal { return 0.0 }
	}

	return 0.0
}

fn from_value_i64(v document.Value) i64 {
	match v {
		document.IntVal { return v.value }
		document.FloatVal { return i64(v.value) }
		document.StringVal { return v.value.i64() }
		document.BoolVal { return if v.value { i64(1) } else { i64(0) } }
		document.NullVal { return i64(0) }
	}

	return i64(0)
}
