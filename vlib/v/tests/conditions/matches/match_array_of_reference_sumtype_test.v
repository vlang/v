type MySumType = []&MyStructA | []&MyStructB

struct MyStructA {}

struct MyStructB {}

fn test_match_branch_with_array_expression() {
	t := MySumType([&MyStructA{}, &MyStructA{}])
	ret := match t {
		[]&MyStructA { '[]&MyStructA' }
		[]&MyStructB { '[]&MyStructB' }
	}
	assert ret == '[]&MyStructA'
}
