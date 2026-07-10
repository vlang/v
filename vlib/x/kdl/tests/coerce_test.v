module main

import x.kdl

fn test_coerce_string_from_quoted() {
	doc := kdl.parse('v "hello"')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'hello'
}

fn test_coerce_string_from_int() {
	doc := kdl.parse('v 42')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == '42'
}

fn test_coerce_string_from_float() {
	doc := kdl.parse('v 3.14')!
	assert kdl.as_string(doc.nodes[0].entries[0].value).len > 0
}

fn test_coerce_string_from_bool() {
	doc := kdl.parse('v #true')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'true'
}

fn test_coerce_string_from_null() {
	doc := kdl.parse('v #null')!
	assert kdl.as_string(doc.nodes[0].entries[0].value) == 'null'
}

fn test_coerce_int_from_int() {
	doc := kdl.parse('v 42')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 42
}

fn test_coerce_int_from_float() {
	doc := kdl.parse('v 3.14')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 3
}

fn test_coerce_int_from_string() {
	doc := kdl.parse('v "42"')!
	assert kdl.as_int(doc.nodes[0].entries[0].value) == 42
}

fn test_coerce_i64() {
	doc := kdl.parse('v -42')!
	assert kdl.as_i64(doc.nodes[0].entries[0].value) == -42
}

fn test_coerce_f64_from_float() {
	doc := kdl.parse('v 3.14')!
	assert kdl.as_f64(doc.nodes[0].entries[0].value) == 3.14
}

fn test_coerce_f64_from_int() {
	doc := kdl.parse('v 42')!
	assert kdl.as_f64(doc.nodes[0].entries[0].value) == 42.0
}

fn test_coerce_bool_true() {
	doc := kdl.parse('v #true')!
	assert kdl.as_bool(doc.nodes[0].entries[0].value) == true
}

fn test_coerce_bool_false() {
	doc := kdl.parse('v #false')!
	assert kdl.as_bool(doc.nodes[0].entries[0].value) == false
}

fn test_coerce_bool_from_int_nonzero() {
	doc := kdl.parse('v 1')!
	assert kdl.as_bool(doc.nodes[0].entries[0].value) == true
}

fn test_coerce_bool_from_int_zero() {
	doc := kdl.parse('v 0')!
	assert kdl.as_bool(doc.nodes[0].entries[0].value) == false
}

fn test_coerce_bool_from_string() {
	doc := kdl.parse('v "true"')!
	assert kdl.as_bool(doc.nodes[0].entries[0].value) == true
}

fn test_coerce_is_null_true() {
	doc := kdl.parse('v #null')!
	assert kdl.is_null(doc.nodes[0].entries[0].value) == true
}

fn test_coerce_is_null_false() {
	doc := kdl.parse('v 42')!
	assert kdl.is_null(doc.nodes[0].entries[0].value) == false
}

fn test_coerce_u64() {
	doc := kdl.parse('v 255')!
	assert kdl.as_u64(doc.nodes[0].entries[0].value) == 255
}

fn test_coerce_numeric_int() {
	doc := kdl.parse('v 100')!
	iv, fv, is_int := kdl.as_numeric(doc.nodes[0].entries[0].value)
	assert is_int == true
	assert iv == 100
	assert fv == 100.0
}

fn test_coerce_numeric_float() {
	doc := kdl.parse('v 3.14')!
	_, _, is_int := kdl.as_numeric(doc.nodes[0].entries[0].value)
	assert is_int == false
}

fn test_property_exists() {
	doc := kdl.parse('config port=8080 host="localhost"')!
	n := doc.nodes[0]
	assert kdl.property_exists(&n, 'port') == true
	assert kdl.property_exists(&n, 'missing') == false
}

fn test_property_get() {
	doc := kdl.parse('config port=8080')!
	n := doc.nodes[0]
	val := kdl.property_get(&n, 'port') or {
		assert false
		return
	}
	assert kdl.as_int(val) == 8080
}

fn test_property_get_rightmost_wins() {
	doc := kdl.parse('config port=8080 port=9090')!
	n := doc.nodes[0]
	val := kdl.property_get(&n, 'port') or {
		assert false
		return
	}
	assert kdl.as_int(val) == 9090
}

fn test_property_has() {
	doc := kdl.parse('config port=8080')!
	assert kdl.property_has(&doc.nodes[0]) == true
}

fn test_property_has_none() {
	doc := kdl.parse('config 1 2 3')!
	assert kdl.property_has(&doc.nodes[0]) == false
}
