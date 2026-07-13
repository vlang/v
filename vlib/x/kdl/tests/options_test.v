module main

import x.kdl
import os

fn test_relaxed_nginx_slash() {
	mut relaxed := kdl.RelaxedNonCompliant{
		flags: kdl.nginx_syntax
	}
	opts := kdl.ParseOpts{
		relaxed: relaxed
	}
	doc := kdl.parse_opts('allow from "192.168.1.1/24"', opts)!
	assert doc.nodes[0].name == 'allow'
	assert doc.nodes[0].entries.len > 0
}

fn test_relaxed_nginx_parens() {
	mut relaxed := kdl.RelaxedNonCompliant{
		flags: kdl.nginx_syntax
	}
	opts := kdl.ParseOpts{
		relaxed: relaxed
	}
	doc := kdl.parse_opts('server (parent)host', opts)!
	assert doc.nodes.len == 1
}

fn test_relaxed_flags_available() {
	_ := kdl.nginx_syntax
	_ := kdl.yaml_toml_assignments
	_ := kdl.multiplier_suffixes
	assert true
}

fn test_relaxed_permit() {
	mut r := kdl.RelaxedNonCompliant{
		flags: kdl.nginx_syntax | kdl.multiplier_suffixes
	}
	r.flags = r.flags | kdl.yaml_toml_assignments
	assert true
}

fn test_relaxed_yaml_toml_colon_assignment() {
	opts := kdl.ParseOpts{
		relaxed: kdl.RelaxedNonCompliant{
			flags: kdl.yaml_toml_assignments
		}
	}
	doc := kdl.parse_opts('node key:value', opts)!
	assert doc.nodes.len == 1
	assert kdl.property_exists(&doc.nodes[0], 'key')
	val := kdl.property_get(&doc.nodes[0], 'key') or {
		assert false
		return
	}
	assert kdl.as_string(val) == 'value'
}

fn test_parse_opts_default() {
	opts := kdl.ParseOpts{}
	doc := kdl.parse_opts('node "val"', opts)!
	assert doc.nodes.len == 1
}

fn test_parse_opts_parse_comments() {
	mut opts := kdl.ParseOpts{
		parse_comments: true
	}
	doc := kdl.parse_opts('node "val"', opts)!
	assert doc.nodes.len == 1
}

fn test_parse_file_opts() {
	tmp := os.join_path(os.temp_dir(), 'kdl_test_parse_file_opts.kdl')
	os.write_file(tmp, 'node1 "a"\nnode2 "b"')!
	opts := kdl.ParseOpts{}
	doc := kdl.parse_file_opts(tmp, opts)!
	assert doc.nodes.len == 2
	os.rm(tmp)!
}

fn test_token_kinds_exist() {
	_ := kdl.TokenKind.identifier
	_ := kdl.TokenKind.string_val
	_ := kdl.TokenKind.int_val
	_ := kdl.TokenKind.float_val
	_ := kdl.TokenKind.suffixed_decimal
	_ := kdl.TokenKind.bool_val
	_ := kdl.TokenKind.null_val
	_ := kdl.TokenKind.type_annotation
	_ := kdl.TokenKind.equals
	_ := kdl.TokenKind.l_brace
	_ := kdl.TokenKind.r_brace
	_ := kdl.TokenKind.semicolon
	_ := kdl.TokenKind.slashdash
	_ := kdl.TokenKind.newline
	_ := kdl.TokenKind.eof
	assert true
}
