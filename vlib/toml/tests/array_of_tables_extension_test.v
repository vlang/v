import toml
import toml.to

// Table headers (`[a.b]`, `[[a.b]]`) whose leading segments name an array of tables
// descend into its last table. The last segment of a `[a.b]` header must name a table,
// never an array of tables. The dotted keys of key/value pairs (`a.b = 1`) never
// descend: a segment naming an array of tables, or a static array, cannot be extended
// by a dotted key.
const invalid_dotted_keys = [
	// toml-test `invalid/array/extend-defined-aot.toml`
	'[[tab.arr]]\n[tab]\narr.val1=1',
	'[[a.b]]\n[a]\nb.c=1',
	'[[a.b]]\n[a]\nb.c.d=1',
	'[t]\n[[t.a.b]]\n[t.a]\nb.c=1',
	'a=[{}]\na.b=1',
	'[t]\na=[{}]\na.b=1',
	'x={a=[{}], a.b=1}',
	'[[a]]\nb=[{x=1}]\nb.c=1',
	'[[a]]\n[[a.b]]\nc=[{}]\nc.d=1',
	'[[a]]\n[[a.b]]\n[a.b.c]\nd=[{}]\nd.e=1',
]

const invalid_table_headers = [
	// toml-test `invalid/table/duplicate-key-07.toml`
	'[[tbl]]\n[tbl]',
	'[[tbl]]\n[tbl]\nx=1',
	'[[a.b]]\n[a.b]',
	'[[a.b]]\n[a.b]\nx=1',
	'[t]\n[[t.a]]\n[t.a]',
	'[[a]]\n[[a.b]]\n[a.b]',
]

const valid_documents = {
	'[[a]]\nb.c=1':                             '{ "a": [ { "b": { "c": 1 } } ] }'
	'[[arr]]\n[[arr]]\narr.val1=1':             '{ "arr": [ { }, { "arr": { "val1": 1 } } ] }'
	'[[tab.arr]]\n[tab]\nx.y=1':                '{ "tab": { "arr": [ { } ], "x": { "y": 1 } } }'
	'[[fruit]]\n[fruit.physical]\ncolor="red"': '{ "fruit": [ { "physical": { "color": "red" } } ] }'
	'[[a]]\n[a.b]\nc.d=1':                      '{ "a": [ { "b": { "c": { "d": 1 } } } ] }'
	'[[a]]\n[a.b.c]\nd.e=1':                    '{ "a": [ { "b": { "c": { "d": { "e": 1 } } } } ] }'
	'[[a.b]]\n[a.b.c]\nd.e=1':                  '{ "a": { "b": [ { "c": { "d": { "e": 1 } } } ] } }'
	'[[a]]\n[[a.b]]\nc.d=1':                    '{ "a": [ { "b": [ { "c": { "d": 1 } } ] } ] }'
	'[[a]]\n[[a.b]]\n[a.b.c]\nd.e=1':           '{ "a": [ { "b": [ { "c": { "d": { "e": 1 } } } ] } ] }'
	'[[a]]\n[[a]]\nb.c=1\n[a.d]\ne.f=1':        '{ "a": [ { }, { "b": { "c": 1 }, "d": { "e": { "f": 1 } } } ] }'
	'x={a.b=1, a.c={d=2}}':                     '{ "x": { "a": { "b": 1, "c": { "d": 2 } } } }'
	'x={a=[{}]}':                               '{ "x": { "a": [ { } ] } }'
	'[t]\na.b.c=1\n[t.a.d]\ne=1':               '{ "t": { "a": { "b": { "c": 1 }, "d": { "e": 1 } } } }'
}

fn assert_parse_error(source string) {
	if doc := toml.parse_text(source) {
		assert false, 'expected an error for `${source}`, got ${to.json(doc)}'
	}
}

fn test_dotted_keys_do_not_extend_arrays() {
	for source in invalid_dotted_keys {
		assert_parse_error(source)
	}
}

fn test_table_headers_do_not_redeclare_arrays_of_tables() {
	for source in invalid_table_headers {
		assert_parse_error(source)
	}
}

fn test_keys_and_headers_below_arrays_of_tables() {
	for source, expected in valid_documents {
		doc := toml.parse_text(source) or {
			assert false, 'unexpected error for `${source}`: ${err}'
			continue
		}
		assert to.json(doc) == expected, source
	}
}
