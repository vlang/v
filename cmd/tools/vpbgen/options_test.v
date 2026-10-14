module main

// Option parsing for the tool's command line: what each flag sets, and which
// inputs are rejected before a .proto file is ever read.

struct OptionCase {
	name string
	raw  []string
	want PbgenOptions
}

const option_cases = [
	OptionCase{'command token is dropped', ['pbgen', 'a.proto'], PbgenOptions{
		inputs: ['a.proto']
	}}
	OptionCase{'vpbgen token is dropped too', ['vpbgen', 'a.proto'], PbgenOptions{
		inputs: ['a.proto']
	}}
	OptionCase{'a bare -- is dropped', ['--', 'a.proto'], PbgenOptions{
		inputs: ['a.proto']
	}}
	OptionCase{'several inputs keep their order', ['a.proto', 'b.proto'], PbgenOptions{
		inputs: ['a.proto', 'b.proto']
	}}
	OptionCase{'all flags at once', ['-v', '-check', '-m', 'kv', '-o', 'c.v', '-grpc', 's.v', 'a.proto'], PbgenOptions{
		inputs:      ['a.proto']
		module_name: 'kv'
		codec_out:   'c.v'
		grpc_out:    's.v'
		check:       true
		verbose:     true
	}}
	OptionCase{'long spellings of the same flags', ['-module', 'kv', '-out', 'c.v', 'a.proto'], PbgenOptions{
		inputs:      ['a.proto']
		module_name: 'kv'
		codec_out:   'c.v'
	}}
	OptionCase{'help needs no input file', ['-h'], PbgenOptions{
		help: true
	}}
	OptionCase{'a flag value is consumed even when it starts with a dash', ['-o', '-weird.v', '-grpc',
		'-svc.v', 'a.proto'], PbgenOptions{
		inputs:    ['a.proto']
		codec_out: '-weird.v'
		grpc_out:  '-svc.v'
	}},
]

fn test_parse_options_accepted() {
	for c in option_cases {
		got := parse_options(c.raw) or { panic(err) }
		assert got == c.want, '${c.name}: got ${got}'
	}
}

// option_error returns the message parse_options rejects `raw` with, or an
// empty string when it accepts it.
fn option_error(raw []string) string {
	parse_options(raw) or { return err.msg() }
	return ''
}

fn test_parse_options_rejects_unknown_flag() {
	msg := option_error(['-nope', 'a.proto'])
	assert msg.contains('unknown option `-nope`'), msg
	assert msg.contains('usage: v pbgen'), msg
}

fn test_parse_options_rejects_missing_input() {
	assert option_error([]).contains('no .proto file given')
	// The flags alone are still not an input.
	assert option_error(['-check', '-v']).contains('no .proto file given')
}

fn test_parse_options_rejects_non_identifier_module_name() {
	// A module name that is a V keyword, or one containing a path separator,
	// would produce a file V cannot import. An empty name is *not* rejected
	// here: it means "derive the module name from the package".
	for bad in ['type', 'match', 'google.rpc', 'a/b', 'a-b'] {
		msg := option_error(['-m', bad, 'a.proto'])
		assert msg.contains('cannot be a V module name'), 'for module name `${bad}`: ${msg}'
	}
}

fn test_is_v_module_name() {
	for good in ['kv', 'google_rpc', 'a', 'module_v1', 'bool', 'kv1'] {
		assert is_v_module_name(good), 'expected `${good}` to be a usable module name'
	}
	for bad in ['', 'type', 'fn', 'google.rpc', 'a/b', 'a-b', 'my mod'] {
		assert !is_v_module_name(bad), 'expected `${bad}` not to be a usable module name'
	}
}

// NOTE: current behaviour, and a real gap. `v_keywords` in namer.v omits names
// that `token.is_keyword()` does treat as keywords, so neither is_v_keyword nor
// safe_field_name/safe_type_name escape them. The compiler rejects
// `true := 5` with "non-name `true` on left side of `:=`", so a proto field or
// message named after one of them yields generated V that does not compile.
// These assertions record today's answers so the list can be extended and this
// test updated when it is.
fn test_names_missing_from_v_keywords() {
	missing := ['true', 'nil', 'union', 'dump', 'global', 'shared', 'isreftype', 'likely', 'unlikely',
		'offsetof']
	for name in missing {
		assert !is_v_keyword(name), '`${name}` is expected to be missing from v_keywords today'
		assert safe_field_name(name) == name, '`${name}` is expected not to be escaped today'
	}
}

fn test_usage_lists_every_flag() {
	u := usage()
	assert u.starts_with('usage: v pbgen [options] <file.proto>...'), u
	for flag in ['-m <name>', '-o <file>', '-grpc <file>', '-check', '-v', '-h, --help'] {
		assert u.contains(flag), 'usage text is missing ${flag}'
	}
	assert u.contains('v pbgen -m kv -o kv/codec.v kv.proto'), u
	assert u.contains('rather than accepted: a `required` field'), u
}
