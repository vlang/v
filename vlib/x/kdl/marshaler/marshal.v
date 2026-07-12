module marshaler

import strings
import x.kdl.document
import math
import x.kdl.generator

pub interface Marshaler {
	to_kdl() string
}

pub struct EncodeOpts {
pub mut:
	rename_all   string
	bare_strings bool
}

pub fn encode[T](val T) string {
	mut opts := EncodeOpts{}
	return encode_opts(val, opts)
}

pub fn encode_opts[T](val T, opts EncodeOpts) string {
	$if T is Marshaler {
		return val.to_kdl()
	}
	$if T is $struct {
		mut doc := document.Document{}
		doc.nodes << encode_struct(val, opts)
		return generator.format(doc) or { '' }
	}
	mut sb := strings.new_builder(128)
	append_value_to_builder(mut sb, to_value(val), opts)
	return sb.str()
}

fn append_value_to_builder(mut sb strings.Builder, v document.Value, opts EncodeOpts) {
	match v {
		document.StringVal {
			match v.flag {
				.raw {
					sb.write_string(document.raw_string(v.value))
				}
				.quoted {
					sb.write_string(document.quote_string(v.value))
				}
				.bare {
					if document.can_be_bare_identifier(v.value) || opts.bare_strings {
						sb.write_string(v.value)
					} else {
						sb.write_string(document.quote_string(v.value))
					}
				}
				else {
					sb.write_string(document.quote_string(v.value))
				}
			}
		}
		document.IntVal {
			match v.flag {
				.hex {
					sb.write_string('0x')
					sb.write_string(v.value.hex())
				}
				.octal {
					sb.write_string('0o')
					sb.write_string(format_oct(v.value))
				}
				.binary {
					sb.write_string('0b')
					sb.write_string(format_bin(v.value))
				}
				else {
					sb.write_string(v.value.str())
				}
			}
		}
		document.FloatVal {
			if math.is_inf(v.value, 0) {
				if v.value > 0 {
					sb.write_string('#inf')
				} else {
					sb.write_string('#-inf')
				}
			} else if math.is_nan(v.value) {
				sb.write_string('#nan')
			} else if v.flag == .scientific {
				sb.write_string(v.value.strsci(6))
			} else {
				sb.write_string(v.value.str())
			}
		}
		document.BoolVal {
			if v.value {
				sb.write_string('#' + 'true')
			} else {
				sb.write_string('#' + 'false')
			}
		}
		document.NullVal {
			sb.write_string('#' + 'null')
		}
	}
}

fn encode_struct[T](val T, opts EncodeOpts) document.Node {
	mut node := document.Node{}
	node.name = apply_rename(typeof[T]().name, opts.rename_all)

	$for field in T.fields {
		tag := kdl_tag(field.attrs)
		_, attrs := parse_kdl_tag(tag)
		if 'arg' in attrs {
			if 'omitempty' !in attrs || !is_field_zero(val.$(field.name)) {
				add_argument_field(mut node, val.$(field.name))
			}
		}
	}
	$for field in T.fields {
		tag := kdl_tag(field.attrs)
		_, attrs := parse_kdl_tag(tag)
		if 'args' in attrs {
			if 'omitempty' !in attrs || !is_field_zero(val.$(field.name)) {
				add_args_field(mut node, val.$(field.name))
			}
		}
	}
	$for field in T.fields {
		tag := kdl_tag(field.attrs)
		tag_name, attrs := parse_kdl_tag(tag)
		if 'arg' !in attrs && 'args' !in attrs {
			skip := 'omitempty' in attrs && is_field_zero(val.$(field.name))
			if !skip {
				fname := if tag_name != '' {
					tag_name
				} else {
					apply_rename(field.name, opts.rename_all)
				}
				if 'props' in attrs {
					add_props_field(mut node, val.$(field.name))
				} else if 'children' in attrs || 'child' in attrs {
					add_child_field(mut node, fname, val.$(field.name), opts)
				} else {
					add_property_field(mut node, fname, val.$(field.name))
				}
			}
		}
	}
	return node
}

fn add_argument_field[T](mut node document.Node, val T) {
	node.entries << document.Argument{
		value: to_value(val)
	}
}

fn add_args_field[T](mut node document.Node, val T) {
	$if T is $array {
		for elem in val {
			node.entries << document.Argument{
				value: to_value(elem)
			}
		}
	}
}

fn add_property_field[T](mut node document.Node, key string, val T) {
	$if T is $array {
		for elem in val {
			node.entries << document.Property{
				key:   key
				value: to_value(elem)
			}
		}
	} $else {
		node.entries << document.Property{
			key:   key
			value: to_value(val)
		}
	}
}

fn add_props_field[T](mut node document.Node, val T) {
	$if T is $map {
		for k, v in val {
			node.entries << document.Property{
				key:   '${k}'
				value: to_value(v)
			}
		}
	}
}

fn add_child_field[T](mut node document.Node, name string, val T, opts EncodeOpts) {
	$if T is $struct {
		mut child := encode_struct(val, opts)
		child.name = name
		node.children << child
	} $else $if T is $array {
		mut child := document.Node{
			name: name
		}
		for elem in val {
			child.entries << document.Argument{
				value: to_value(elem)
			}
		}
		node.children << child
	} $else $if T is $map {
		for k, v in val {
			mut child := document.Node{
				name: '${k}'
			}
			child.entries << document.Argument{
				value: to_value(v)
			}
			node.children << child
		}
	} $else {
		node.children << document.Node{
			name:    name
			entries: [document.Argument{ value: to_value(val) }]
		}
	}
}

pub struct Encoder {
pub mut:
	opts EncodeOpts
	sb   strings.Builder
}

pub fn new_encoder() Encoder {
	return Encoder{
		sb: strings.new_builder(256)
	}
}

pub fn (mut e Encoder) encode[T](val T) string {
	e.sb = strings.new_builder(256)
	str := encode_opts(val, e.opts)
	e.sb.write_string(str)
	return str
}

pub fn (mut e Encoder) str() string {
	return e.sb.str()
}
