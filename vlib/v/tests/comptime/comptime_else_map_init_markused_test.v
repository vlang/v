// Regression test for https://github.com/vlang/v/issues/XXXXX
// Comptime $else branch with map literal init should not cause
// a panic in the markused walker when the branch is skipped.
import x.json2

pub enum AppError {
	invalid_method
}

pub fn new_error_detail_with_details(code AppError, message string, details map[string]string) !string {
	return '${message}: ${details}'
}

struct App {}

fn (mut app App) test_handler(message map[string]json2.Any) string {
	return 'test result'
}

pub fn fire_call[T](mut app T, method_name string, message map[string]json2.Any) !string {
	$for method in T.methods {
		if method.name == method_name {
			$if method.return_type is string {
				return app.$method(message)
			} $else {
				return new_error_detail_with_details(.invalid_method, 'Method should return string',
					{
					'method':      method_name
					'return_type': method.return_type
				})
			}
		}
	}
	return error('not found')
}

fn test_comptime_else_map_init() {
	mut app := App{}
	message := map[string]json2.Any{}
	result := fire_call(mut app, 'test_handler', message) or {
		assert false, 'unexpected error: ${err}'
		return
	}
	assert result == 'test result'
}
