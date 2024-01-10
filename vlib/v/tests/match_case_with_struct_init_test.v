struct Get {}

struct Post {}

struct Part {
	value string
}

type RoutePart = Get | Part | Post

fn test_match_case_with_struct_init() {
	route := [RoutePart(Get{}), RoutePart(Part{
		value: '/'
	})]

	status_code := match route {
		[RoutePart(Get{}), RoutePart(Part{
			value: '/'
		})] {
			'200'
		}
		else {
			'404'
		}
	}

	print(status_code)
	assert status_code == '200'
}
