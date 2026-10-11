module token

// The scanner reads short keywords straight out of this table, so a keyword that
// falls through to `.name` stops being a keyword everywhere.
fn test_from_string_tinyv_recognises_every_keyword_it_tables() {
	keywords := {
		'as':         Token.key_as
		'fn':         Token.key_fn
		'go':         Token.key_go
		'if':         Token.key_if
		'in':         Token.key_in
		'is':         Token.key_is
		'or':         Token.key_or
		'asm':        Token.key_asm
		'for':        Token.key_for
		'mut':        Token.key_mut
		'nil':        Token.key_nil
		'pub':        Token.key_pub
		'dump':       Token.key_dump
		'else':       Token.key_else
		'enum':       Token.key_enum
		'goto':       Token.key_goto
		'lock':       Token.key_lock
		'none':       Token.key_none
		'true':       Token.key_true
		'type':       Token.key_type
		'break':      Token.key_break
		'const':      Token.key_const
		'defer':      Token.key_defer
		'false':      Token.key_false
		'match':      Token.key_match
		'rlock':      Token.key_rlock
		'spawn':      Token.key_spawn
		'union':      Token.key_union
		'assert':     Token.key_assert
		'atomic':     Token.key_atomic
		'import':     Token.key_import
		'module':     Token.key_module
		'return':     Token.key_return
		'select':     Token.key_select
		'shared':     Token.key_shared
		'sizeof':     Token.key_sizeof
		'static':     Token.key_static
		'struct':     Token.key_struct
		'typeof':     Token.key_typeof
		'unsafe':     Token.key_unsafe
		'__global':   Token.key_global
		'_likely_':   Token.key_likely
		'_unlikely_': Token.key_unlikely
		'continue':   Token.key_continue
		'volatile':   Token.key_volatile
		'interface':  Token.key_interface
		'isreftype':  Token.key_isreftype
		'__offsetof': Token.key_offsetof
	}
	for name, expected in keywords {
		assert Token.from_string_tinyv(name) == expected, '`${name}` should be a keyword'
	}
}

// Only names of exactly the tabled lengths can be keywords, and a name that is
// close to one has to fall through to `.name`.
fn test_from_string_tinyv_falls_back_to_name_for_everything_else() {
	for name in ['', 'a', 'ab', 'abc', 'abcd', 'abcde', 'abcdef', 'abcdefg', 'abcdefgh', 'abcdefghi',
		'abcdefghij', 'abcdefghijk', 'ifs', 'iff', 'fnn', 'aas', 'ass', 'elso', 'dumps', 'True',
		'MUT', 'Pub', 'fns', 'gos', 'os', 'ins', 'iss', 'ors', 'asmx', 'forr', 'mute', 'nils',
		'pubx', 'els', 'enu', 'got', 'loc', 'non', 'tru', 'typ', 'brea', 'cons', 'defe', 'fals',
		'matc', 'rloc', 'spaw', 'unio', 'asse', 'ato', 'impor', 'modul', 'retur', 'selec', 'share',
		'sizeo', 'stati', 'struc', 'typeo', 'unsaf', '__globa', '_likely', '__likely', '_unlikely',
		'__unlikely', 'continu', 'volatil', 'interfac', 'isreftyp', '__offseto', 'returnx', 'continuee',
		'interfacex'] {
		assert Token.from_string_tinyv(name) == Token.name, '`${name}` should be a plain name'
	}
}
