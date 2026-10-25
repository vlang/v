module transform

import v.flat
import v.types

fn test_numeric_alias_normalization_preserves_one_step_and_recursive_results() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	leaf_id := flat.source_type_id('Leaf')
	tc.type_aliases.set_id('Leaf', flat.source_type_int)
	tc.type_aliases.set_id('Bare', leaf_id)
	tc.type_aliases.set_id('current.Local', leaf_id)
	tc.type_aliases.set_id('dep.Exact', leaf_id)
	tc.file_imports[file_import_key('alias.v', 'renamed')] = 'dep'
	tc.structs['dep.Canonical'] = []types.StructField{}
	mut t := Transformer{
		tc:         &tc
		cur_file:   'alias.v'
		cur_module: 'current'
	}
	t.type_alias_suffixes['SuffixOnly'] = leaf_id
	// Bare, module-qualified fallback and suffix aliases return their direct target.
	for name in ['Bare', 'Local', 'SuffixOnly'] {
		assert t.normalize_type_alias_uncached(flat.source_type_id(name)) == leaf_id
	}
	// Exact qualified aliases and aliases found through imports recurse.
	for name in ['dep.Exact', 'renamed.Exact'] {
		assert t.normalize_type_alias_uncached(flat.source_type_id(name)) == flat.source_type_int
	}
	canonical_id := flat.source_type_id('dep.Canonical')
	assert t.normalize_type_alias_uncached(canonical_id) == canonical_id
	assert t.normalize_type_alias_uncached(flat.source_type_id('renamed.Canonical')) == canonical_id
	// A declared canonical type takes precedence over a colliding import alias.
	tc.file_imports[file_import_key('alias.v', 'dep')] = 'unrelated'
	assert t.normalize_type_alias_uncached(canonical_id) == canonical_id
}

fn test_numeric_alias_normalization_preserves_composites_and_malformed_spellings() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('Bare', flat.source_type_id('Leaf'))
	tc.type_aliases.set_id('Leaf', flat.source_type_int)
	tc.type_aliases.set_id('Empty', 0)
	uncached := Transformer{
		tc: &tc
	}
	cached := Transformer{
		tc:          &tc
		alias_cache: &AliasCache{}
	}
	cases := {
		'':                ''
		'Bare':            'Leaf'
		'Empty':           ''
		'mut Bare':        '&Leaf'
		'&Bare':           '&Leaf'
		'[]Bare':          '[]Leaf'
		'chan Bare':       'chan Leaf'
		'[2]Bare':         '[2]Leaf'
		'map[Bare][]Bare': 'map[Leaf][]Leaf'
		'?Bare':           '?Leaf'
		'!Bare':           '!Leaf'
		'&void[5]':        'voidptr[5]'
		'&void[5':         '&void[5'
		'T':               'T'
		'[':               '['
		'[2':              '[2'
		'map[':            'map['
		'map[Bare':        'map[Bare'
		'...Bare':         '...Bare'
		'fn (Bare) Bare':  'fn (Bare) Bare'
	}
	for input, expected in cases {
		input_id := flat.source_type_id(input)
		expected_id := flat.source_type_id(expected)
		assert uncached.normalize_type_alias(input_id) == expected_id, input
		for _ in 0 .. 3 {
			assert cached.normalize_type_alias(input_id) == expected_id, input
		}
	}
}
