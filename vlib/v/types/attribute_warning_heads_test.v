module types

fn test_attribute_warning_heads_distinguish_names_from_metadata_and_arguments() {
	source := "@[foo; foo_bar; Call('first', named: 'value', 'second'); 'legacy']\nfn example() {}"
	heads := attribute_warning_heads(source)
	assert heads.map(it.name) == ['foo', 'foo_bar', 'Call']
	for head in heads {
		assert source[head.offset..head.offset + head.name.len] == head.name
		assert head.declaration_offset == source.index('fn example') or { -1 }
		assert head.declaration_end == source.index('{') or { -1 }
	}
}

fn test_attribute_warning_heads_ignore_quoted_markers_and_comments() {
	source := "const marker = '@[fake]'\n/* @[also_fake] */\n@[real]\n// @[comment]\n@['quoted']\npub struct Example {}"
	heads := attribute_warning_heads(source)
	assert heads.len == 1
	assert heads[0].name == 'real'
	assert heads[0].declaration_offset == source.index('struct Example') or { -1 }
}

fn test_attribute_warning_heads_keep_declaration_ownership_and_qualified_names() {
	source := "@[legacy]\nstruct Bare {}\n@['legacy']\nstruct Quoted {}\n@[mod.Unknown(key: 'value')]\nfn example() {}"
	heads := attribute_warning_heads(source)
	assert heads.map(it.name) == ['legacy', 'mod.Unknown']
	assert heads[0].declaration_offset == source.index('struct Bare') or { -1 }
	assert heads[1].declaration_offset == source.index('fn example') or { -1 }
	assert heads[0].declaration_end < source.index('struct Quoted') or { -1 }
	assert heads[1].declaration_end > source.index('example()') or { -1 }
}
