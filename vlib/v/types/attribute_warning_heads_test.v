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

fn test_attribute_warning_heads_keep_leading_and_trailing_child_ownership() {
	source := "struct Fields {\n@[lead]\nvalue int @[tail]\n}\nenum Values {\none [old]\n@['old']\ntwo\n[old_lead]\nthree\n}"
	heads := attribute_warning_heads(source)
	assert heads.map(it.name) == ['lead', 'tail', 'old', 'old_lead']
	assert heads[0].leading
	assert heads[0].declaration_offset == source.index('value int') or { -1 }
	assert !heads[1].leading
	assert !heads[2].leading
	assert heads[2].legacy
	assert heads[2].previous_offset == source.index('one [old]') or { -1 }
	assert heads[3].leading
	assert heads[3].declaration_offset == source.index('three') or { -1 }
}

fn test_attribute_warning_heads_keep_inline_field_access_modifiers() {
	source := 'struct Fields {\n@[before_mut]\nmut first int\nmut @[after_mut] second int\n@[before_pub_mut]\npub mut third int\npub mut @[after_pub_mut] fourth int\n@[before_module_mut]\npub module_mut fifth int\npub @[after_pub] sixth int\n}'
	heads := attribute_warning_heads(source)
	assert heads.map(it.name) == ['before_mut', 'after_mut', 'before_pub_mut', 'after_pub_mut',
		'before_module_mut', 'after_pub']
	for i, field in ['first', 'second', 'third', 'fourth', 'fifth', 'sixth'] {
		assert heads[i].leading
		assert heads[i].declaration_offset == source.index('${field} int') or { -1 }
	}
}
