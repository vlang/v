struct API_error {
pub mut:
	errors []string
}

fn delete_secret_v1() API_error {
	response := req_do() or {
		match err.msg() {
			'dial_tcp failed' {
				return API_error{
					errors: ['Vault server not started']
				}
			}
			else {
				return API_error{
					errors: [err.msg()]
				}
			}
		}
	}
	println(response)
}

fn req_do() ?string {
	return error('dial_tcp failed')
}

fn test_or_expr_with_nested_match_expr() {
	err := delete_secret_v1()
	println(err)
	assert err.errors.len == 1
	assert err.errors[0] == 'Vault server not started'
}
