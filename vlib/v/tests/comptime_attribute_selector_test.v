[test: 'hello']
[abc]
struct Test {}

fn test_attributes() {
	$for attr in Test.attributes {
		if attr.has_arg {
			assert attr.name == 'test'
			assert attr.arg == 'hello'
		} else {
			assert attr.name == 'abc'
		}
	}
}
