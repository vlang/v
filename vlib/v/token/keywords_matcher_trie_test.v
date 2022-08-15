import v.token

fn test_new_keywords_matcher_from_array_trie_works() {
	method_names := ['filter', 'clone', 'repeat', 'reverse', 'map', 'slice', 'sort', 'contains',
		'index', 'wait', 'any', 'all', 'first', 'last', 'pop']
	matcher := token.new_keywords_matcher_from_array_trie(method_names)
	for word in [method_names.first(), method_names.last(), 'something', 'another', 'x', 'y', '',
		'---', '123', 'abc.def', 'xyz !@#'] {
		res1 := matcher.find(word) != -1
		res2 := word in method_names
		assert res1 == res2
	}
}
