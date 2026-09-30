// vtest vflags: -d ownership -gc none

type ForwardedResultValue = int | string

struct ForwardedResultError {
	message string
	number  int
}

fn (value ForwardedResultError) msg() string {
	return value.message
}

fn (value ForwardedResultError) code() int {
	return value.number
}

fn forwarded_string_failure() !string {
	return ForwardedResultError{'forwarded failure'.clone(), 47}
}

fn forwarded_string_result(fail bool) (int, !string) {
	if fail {
		return 7, forwarded_string_failure()
	}
	return 7, 'successful payload'.clone()
}

fn converted_string_result(fail bool) (int, !ForwardedResultValue) {
	return forwarded_string_result(fail)
}

fn test_forwarded_result_conversion_preserves_successful_string() {
	number, result := converted_string_result(false)
	assert number == 7
	value := result or { panic(err) }
	assert value is string
	assert value as string == 'successful payload'
}

fn test_forwarded_result_conversion_preserves_error() {
	number, result := converted_string_result(true)
	assert number == 7
	result or {
		assert err is ForwardedResultError
		assert err.msg() == 'forwarded failure'
		assert err.code() == 47
		return
	}
	assert false
}
