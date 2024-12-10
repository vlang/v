@[params]
struct FooParams {
	type FooType = .first
}

enum FooType {
	first
	second
}

fn foo(params FooParams) string {
	match params.type {
		.first {
			return 'First'
		}
		.second {
			return 'Second'
		}
	}
}

fn test_main() {
	assert foo(type: .second) == 'Second'
}
