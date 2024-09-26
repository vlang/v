interface Context {
	str() string
}

struct OtherContext {
	a string
}

pub fn (o &OtherContext) str() string {
	return 'OtherContext'
}

struct WithContext {
pub mut:
	ctx Context
}

fn with_function(ctx Context) {
	println(ctx)
}

fn test_main() {
	mut s := WithContext{
		ctx: OtherContext{}
	}
	assert dump(s) == WithContext{
		ctx: OtherContext{}
	}

	assert dump(OtherContext{}) == OtherContext{}
}
