@[params]
struct ConnectParams {
	password ?string
}

pub fn connect(params ConnectParams) ? {
	password2 := ?string(params.password)
	assert password2? == 'foo'
}

fn test_main() {
	connect(ConnectParams{
		password: 'foo'
	})
}
