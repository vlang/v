import json

pub struct DocumentFindFilter[T] {
pub:
	selector        ?T
	selector2       ?[]T
	selector3       ?[1]T
	limit           ?int
	skip            ?int
	fields          ?[]string // This breaks the compiler when encoding to JSON
	conflicts       ?bool
	read_quorum     ?int @[json: r]
	update          ?bool
	stable          ?bool
	stale           ?string
	execution_stats ?bool
}

fn test_string() {
	t := DocumentFindFilter[string]{
		selector:  'aa'
		selector2: ['a', 'b']
		selector3: ['z']!
	}

	assert json.encode(t) == '{"selector":"aa","selector2":["a","b"],"selector3":["z"]}'
	assert json.decode(DocumentFindFilter[string], '{"selector":"aa","selector2":["a","b"],"selector3":["z"]}')! == t
}

fn test_int() ! {
	t := DocumentFindFilter[int]{
		selector:  1
		selector2: [1, 2]
		selector3: [3]!
	}

	assert json.encode(t) == '{"selector":1,"selector2":[1,2],"selector3":[3]}'
	assert json.decode(DocumentFindFilter[int], '{"selector":1,"selector2":[1,2],"selector3":[3]}')! == t
}

fn test_none() ! {
	t := DocumentFindFilter[int]{}

	assert json.encode(t) == '{}'
	assert json.decode(DocumentFindFilter[int], '{}')! == t
}
